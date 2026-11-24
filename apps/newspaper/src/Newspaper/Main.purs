-- | The newspaper: prints the front page of the issue its backend holds (the engine's live run, newest played round).
module Newspaper.Main (main) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Array (null)
import Data.String (toLower)
import Edition.Calendar (Date, issueDate)
import Edition.FrontPage (Direction(..), FrontPage, Story, Tick, frontPage)
import Edition.Brief (applyCopy)
import Edition.Issue as Issue
import Effect (Effect)
import Control.Monad.Rec.Class (forever)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Exception (throw)
import Newspaper.Client as Client
import React.Basic.DOM as D
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.Hooks (Component, JSX, component, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

data Edition = NoRun | Waiting String | Printed String FrontPage | Trouble String

-- | The backend reads the engine once a minute, so reading the backend more often finds nothing new.
pollMs :: Milliseconds
pollMs = Milliseconds 60000.0

mkPaper :: Component Unit
mkPaper = component "Paper" \_ -> React.do
  edition /\ setEdition <- useState (Waiting "")
  useEffectOnce do
    -- the browser revalidates /api/edition against its cached copy (the backend tags it and asks for no-cache), so
    -- an unchanged issue costs a bodyless 304; the page is rebuilt only when the issue differs from the last one
    shown <- Ref.new Nothing
    launchAff_ $ forever do
      result <- Client.edition
      last <- liftEffect (Ref.read shown)
      case result of
        Right issue | Just issue == last -> pure unit
        Right issue -> liftEffect do
          Ref.write (Just issue) shown
          setEdition \_ -> case issue of
            Issue.NoRun -> NoRun
            Issue.Waiting run -> Waiting run
            Issue.Printed run r copy -> Printed run (maybe identity applyCopy copy (frontPage r))
        -- a backend that cannot be reached leaves a printed page standing
        Left e -> liftEffect $ setEdition \page -> case page of
          Printed _ _ -> page
          _ -> Trouble e
      delay pollMs
    pure (pure unit)
  pure $ D.div { className: "paper", children: [ body edition ] }

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
                [ D.h2 { className: "rule-heading", children: [ D.text "Top stories" ] }
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
                      , D.p { className: "footer-blurb", children: [ D.text "Reporting on the prices that move global markets and the policies that shape trade." ] }
                      ]
                  }
              , column "Sections" (map D.text sections)
              , column "Markets" (map D.text [ "World technology", "World industry", "US technology", "China industry", "US dollar in yuan" ])
              , D.div
                  { className: "footer-edition"
                  , children:
                      [ D.h2 { className: "footer-heading", children: [ D.text "Edition" ] }
                      , D.p { className: "footer-date", children: [ D.text (longDate date) ] }
                      , D.p { className: "footer-note", children: [ D.text "Updated at market close" ] }
                      ]
                  }
              ]
          }
      ]
  }
  where
  date = maybe today (\p -> issueDate p.roundMonths today p.round) page
  -- the sections this edition prints; with no edition yet, every section the paper has
  sections = case page of
    Just p -> map _.kicker (maybe [] pure p.lead <> p.stories)
    Nothing -> [ "Policy", "Markets", "Shortages", "Finance", "Industry", "Households", "Dealmaking" ]
  column heading items = D.div
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
      [ D.div
          { className: "lead-text"
          , children:
              [ kicker s
              , D.h2 { className: "lead-headline", children: [ D.text s.headline ] }
              , D.ul { className: "lead-points", children: map (\l -> D.li_ [ D.text l ]) s.body }
              ]
          }
      , media "media media-lead"
      ]
  }

-- | Where a story's picture will go: an empty frame at the picture's proportions, until the paper has pictures.
media :: String -> JSX
media className = D.div { className, role: "presentation" }

story :: Story -> JSX
story s = D.article
  { className: "story"
  , id: anchor s
  , children:
      [ media "media media-story"
      , kicker s
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

