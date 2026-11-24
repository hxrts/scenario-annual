-- | A front page composed from one round's report. Pure: the same report always makes the same page.
module Edition.FrontPage (Story, FrontPage, Tick, Direction(..), frontPage, direction) where

import Prelude

import Data.Array (filter, sortWith, foldl, length, nub, null, uncons, unsnoc)
import Data.Number (abs, fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Foldable (any)
import Data.String (Pattern(..), Replacement(..), contains, drop, joinWith, replaceAll, stripPrefix, take, toUpper)
import Edition.Names as Names
import Edition.Report (Report)

type Story = { kicker :: String, headline :: String, body :: Array String }

data Direction = Up | Down | Flat

derive instance Eq Direction

instance Show Direction where
  show Up = "Up"
  show Down = "Down"
  show Flat = "Flat"

type Tick = { name :: String, price :: String, change :: String, direction :: Direction, rate :: Boolean }

-- | Which way a display-string change moved: no non-zero digit is flat, a leading minus is down, anything else up.
direction :: String -> Direction
direction c
  | not (any (\d -> contains (Pattern d) c) [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]) = Flat
  | isJust (stripPrefix (Pattern "-") c) || isJust (stripPrefix (Pattern "\x2212") c) = Down
  | otherwise = Up

type FrontPage = { round :: Int, rounds :: Int, roundMonths :: Int, ticker :: Array Tick, lead :: Maybe Story, stories :: Array Story }

frontPage :: Report -> FrontPage
frontPage r =
  case uncons (policy <> markets <> distress <> failures) of
    Nothing -> { round: r.round, rounds: r.rounds, roundMonths: r.roundMonths, ticker, lead: Nothing, stories: [] }
    Just { head, tail } -> { round: r.round, rounds: r.rounds, roundMonths: r.roundMonths, ticker, lead: Just head, stories: tail }
  where
  ticker = map (\p -> { name: Names.tickerName p.exchange, price: Names.priced p.exchange p.price, change: Names.signed p.change, direction: direction p.change, rate: Names.isRate p.exchange }) (sortWith (_.exchange >>> Names.tickerRank) r.prices)

  policy
    | null r.enacted && null r.repealed = []
    | otherwise =
        [ { kicker: "Policy"
          , headline: policyHeadline
          , body: map (\m -> "In force: " <> Names.phrase m) r.enacted <> map (\m -> "Repealed: " <> Names.phrase m) r.repealed
          }
        ]
  policyHeadline
    | null r.repealed = "New rules take effect: " <> listed (map Names.phrase r.enacted)
    | null r.enacted = "Rules repealed: " <> listed (map Names.phrase r.repealed)
    | otherwise = "Policy shake-up: " <> count r.enacted "rule" <> " in, " <> show (length r.repealed) <> " out"

  markets = case biggestMove of
    Nothing -> []
    Just p ->
      [ { kicker: "Markets"
        , headline: moveHeadline p
        , body: map (\q -> Names.exchange q.exchange <> " at " <> Names.pricedProse q.exchange q.price <> " (" <> Names.signed q.change <> ")") r.prices
        }
      ]
  biggestMove = foldl (\best p -> if maybe true (\b -> magnitude p.change > magnitude b.change) best then Just p else best) Nothing r.prices
  moveHeadline p = case direction p.change of
    Up -> Names.exchange p.exchange <> " rises to " <> Names.pricedProse p.exchange p.price
    Down -> Names.exchange p.exchange <> " falls to " <> Names.pricedProse p.exchange p.price
    Flat -> "Markets hold steady in round " <> show r.round

  distress
    | null r.shortfalls = []
    | otherwise =
        [ { kicker: "Shortages"
          , headline: "Shortages hit " <> listed (nub (map (_.actor >>> Names.actor) r.shortfalls))
          , body: map (\x -> capitalise (Names.actor x.actor) <> " is " <> x.short <> " short of " <> Names.resource x.resource) r.shortfalls
          }
        ]

  failures
    | null r.rejected && null r.lapsed = []
    | otherwise =
        [ { kicker: "Dealmaking"
          , headline: capitalise (count (map _.candidateTransaction r.rejected <> r.lapsed) "deal") <> " fall through"
          , body: map (\x -> capitalise (Names.deal x.candidateTransaction) <> " was refused: " <> Names.phrase x.reason) r.rejected
              <> map lapsedLine (nub (map Names.dealOwner r.lapsed))
          }
        ]
  lapsedLine owner = capitalise owner <> ": " <> count (filter (\x -> Names.dealOwner x == owner) r.lapsed) "offer" <> " lapsed unanswered"

count :: forall a. Array a -> String -> String
count xs noun = show (length xs) <> " " <> noun <> (if length xs == 1 then "" else "s")

-- | "a", "a and b", "a, b and c".
listed :: Array String -> String
listed xs = case unsnoc xs of
  Nothing -> ""
  Just { init: [], last } -> last
  Just { init, last } -> joinWith ", " init <> " and " <> last

capitalise :: String -> String
capitalise s = toUpper (take 1 s) <> drop 1 s

-- | How far a display-string change moved, ignoring its sign; text that is not a number (such as "<0.01") is 0.
magnitude :: String -> Number
magnitude c = abs (fromMaybe 0.0 (fromString (replaceAll (Pattern "+") (Replacement "") (replaceAll (Pattern "−") (Replacement "-") c))))
