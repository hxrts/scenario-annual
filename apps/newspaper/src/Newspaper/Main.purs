-- | The newspaper: follows the engine's live run and prints the front page of the latest played round.
module Newspaper.Main (main) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Array (null)
import Data.String (toLower)
import Data.Tuple (Tuple(..))
import Edition.Calendar (Date, issueDate)
import Edition.FrontPage (Direction(..), FrontPage, Story, Tick, frontPage)
import Edition.Report (Report, playedThrough)
import Effect (Effect)
import Control.Monad.Rec.Class (forever)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Newspaper.Client as Client
import React.Basic.DOM as D
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.Hooks (Component, JSX, component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

data Edition = NoRun | Waiting String | Printed String FrontPage | Trouble String

pollMs :: Milliseconds
pollMs = Milliseconds 3000.0

mkPaper :: Component Unit
mkPaper = component "Paper" \_ -> React.do
  edition /\ setEdition <- useState' (Waiting "")
  useEffectOnce do
    launchAff_ $ forever do
      next <- Client.live >>= case _ of
        Left e -> pure (Trouble e)
        Right Nothing -> pure NoRun
        Right (Just l) -> latest l.id (playedThrough l + 1) <#> case _ of
          Left e -> Trouble e
          Right Nothing -> Waiting l.id
          Right (Just r) -> Printed l.id (frontPage r)
      liftEffect (setEdition next)
      delay pollMs
    pure (pure unit)
  pure $ D.div { className: "paper", children: [ body edition ] }

-- | The newest played round's report at or below `round`. The live state's round can lag a round already played
-- | (an all-agent run reports the same round before and after it resolves), so the round itself is tried first and
-- | a 404 falls back to the one before.
latest :: String -> Int -> Aff (Either String (Maybe Report))
latest run round
  | round < 1 = pure (Right Nothing)
  | otherwise = Client.report run round >>= case _ of
      Right Nothing -> latest run (round - 1)
      found -> pure found

-- | Today's date in the reader's browser, and a date written out in full ("Tuesday, 24 November 2026").
foreign import today :: Date
foreign import longDate :: Date -> String

frame :: Array Tick -> Maybe FrontPage -> JSX -> JSX
frame ticks page inner = D.div { className: "page", children: [ tickerBar ticks, masthead page, inner, footer page ] }

tickerBar :: Array Tick -> JSX
tickerBar ticks = D.div
  { className: "ticker"
  , children: [ D.ul_ (map tick ticks) ]
  }
  where
  tick t = D.li
    { className: if t.rate then "rate" else ""
    , children:
        [ D.span { className: "ticker-name", children: [ D.text t.name ] }
        , D.span
            { className: "ticker-figures"
            , children:
                [ D.span
                    { className: "ticker-price"
                    , children: [ D.text t.price ]
                    }
                , D.span { className: "ticker-change " <> tone t.direction, children: [ D.text t.change ] }
                ]
            }
        ]
    }
  tone = case _ of
    Up -> "up"
    Down -> "down"
    Flat -> "flat"

-- | The title bar: a three-column grid with the title in the centre column and the date in the left.
masthead :: Maybe FrontPage -> JSX
masthead page = D.header
  { className: "masthead"
  , children:
      [ D.div
          { className: "masthead-meta"
          , children:
              [ D.span_ [ D.text (longDate (maybe today (\p -> issueDate p.roundMonths today p.round) page)) ] ]
          }
      , D.h1 { className: "masthead-title", children: [ D.text "Scenario Annual" ] }
      ]
  }

body :: Edition -> JSX
body = case _ of
  NoRun -> frame [] Nothing (note "No run is live. The presses are idle.")
  Waiting _ -> frame [] Nothing (note "Going to press: awaiting the first round.")
  Trouble e -> frame [] Nothing (note ("Wire trouble: " <> e))
  Printed _ p -> frame p.ticker (Just p) $ D.main
    { className: "front"
    , children:
        [ maybe (note "A quiet round.") lead p.lead
        , if null p.stories then mempty
          else D.section
            { className: "top"
            , children:
                [ D.h2 { className: "rule-heading", children: [ D.text "More top stories" ] }
                , D.div { className: "columns", children: map story p.stories }
                ]
            }
        ]
    }
  where
  note t = D.p { className: "note", children: [ D.text t ] }

-- | A story's place on the page, named by its section: `#markets`, `#policy`.
anchor :: Story -> String
anchor s = toLower s.kicker

-- | The footer: the paper in dark mode, on a twelve-column grid. The wordmark and what the paper covers, its sections
-- | (each a link to its story on the page), the markets it follows, and the edition's date.
footer :: Maybe FrontPage -> JSX
footer page = D.footer
  { className: "footer"
  , children:
      [ D.div
          { className: "footer-grid"
          , children:
              [ D.div
                  { className: "footer-brand"
                  , children:
                      [ D.p { className: "footer-title", children: [ D.text "Scenario Annual" ] }
                      , D.p { className: "footer-blurb", children: [ D.text "Reporting on global markets, trade and policy: the prices that move, the measures that pass, and the deals that fall through." ] }
                      ]
                  }
              , column "Sections" (map (\(Tuple name href) -> D.a { href, children: [ D.text name ] }) sections)
              , column "Markets" (map D.text [ "World technology", "World industry", "US technology", "China industry", "Dollar–renminbi" ])
              , column "Edition" [ D.text (longDate date), D.text "Updated as markets close" ]
              ]
          }
      ]
  }
  where
  date = maybe today (\p -> issueDate p.roundMonths today p.round) page
  sections = [ Tuple "Policy" "#policy", Tuple "Markets" "#markets", Tuple "Shortages" "#shortages", Tuple "Dealmaking" "#dealmaking" ]
  column heading items = D.nav
    { className: "footer-column"
    , children:
        [ D.h2 { className: "footer-heading", children: [ D.text heading ] }
        , D.ul_ (map (\i -> D.li_ [ i ]) items)
        ]
    }

lead :: Story -> JSX
lead s = D.article
  { className: "lead"
  , id: anchor s
  , children:
      [ kicker s
      , D.h2 { className: "lead-headline", children: [ D.text s.headline ] }
      , D.ul { className: "lead-points", children: map (\l -> D.li_ [ D.text l ]) s.body }
      ]
  }

story :: Story -> JSX
story s = D.article
  { className: "story"
  , id: anchor s
  , children:
      [ kicker s
      , D.h3 { className: "story-headline", children: [ D.text s.headline ] }
      , D.ul { className: "story-points", children: map (\l -> D.li_ [ D.text l ]) s.body }
      ]
  }

kicker :: Story -> JSX
kicker s = D.p { className: "kicker", children: [ D.text s.kicker ] }

main :: Effect Unit
main = do
  root <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  case root of
    Nothing -> throw "no #root"
    Just el -> do
      paper <- mkPaper
      r <- createRoot el
      renderRoot r (paper unit)

