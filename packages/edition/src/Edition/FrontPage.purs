-- | A front page composed from one round's report. Pure: the same report always makes the same page.
module Edition.FrontPage (Story, FrontPage, Tick, Direction(..), frontPage, direction) where

import Prelude

import Data.Array (filter, find, head, index, mapMaybe, sortWith, take, foldl, length, nub, null, uncons, unsnoc)
import Data.Number (abs, fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Traversable (traverse)
import Data.Foldable (any)
import Control.Alt ((<|>))
import Data.String (Pattern(..), Replacement(..), contains, drop, joinWith, replaceAll, stripPrefix, toUpper)
import Data.String as String
import Edition.Names as Names
import Edition.Report (Report)

-- | A story: its section, its headline, a summary the page prints under it, and the facts it was drawn from (which a
-- | writer is given). Until a writer's copy arrives the summary is one plain sentence made from the facts.
type Story = { kicker :: String, headline :: String, summary :: String, body :: Array String }

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
  case uncons (policy <> markets <> distress <> finance <> industry <> households <> failures) of
    Nothing -> { round: r.round, rounds: r.rounds, roundMonths: r.roundMonths, ticker, lead: Nothing, stories: [] }
    Just { head, tail } -> { round: r.round, rounds: r.rounds, roundMonths: r.roundMonths, ticker, lead: Just head, stories: take topStories tail }
  where
  ticker = map (\p -> { name: Names.tickerName p.exchange, price: Names.priced p.exchange p.price, change: Names.signed p.change, direction: direction p.change, rate: Names.isRate p.exchange }) (sortWith (_.exchange >>> Names.tickerRank) r.prices)

  policy
    | null r.enacted && null r.repealed = []
    | otherwise =
        [ { kicker: "Policy"
          , headline: policyHeadline
          , summary: policySummary
          , body: measureLines "In force" r.enacted <> measureLines "Repealed" r.repealed
          }
        ]
  policyHeadline
    | null r.repealed = maybe ("New rules take effect: " <> listed (map Names.phrase r.enacted)) enactedHeadline (owners r.enacted)
    | null r.enacted = maybe ("Rules repealed: " <> listed (map Names.phrase r.repealed)) repealedHeadline (owners r.repealed)
    | otherwise = "Policy shake-up: " <> count r.enacted "measure" <> " in, " <> show (length r.repealed) <> " out"
  repealedHeadline who
    | length r.repealed == 1 = "A measure from " <> listed who <> " is repealed"
    | otherwise = show (length r.repealed) <> " measures from " <> listed who <> " are repealed"
  policySummary
    | null r.repealed = maybe (count r.enacted "new measure" <> " came into force.") (\who -> capitalise (listed who) <> " brought " <> measures r.enacted <> " into force; it holds until repealed.") (owners r.enacted)
    | null r.enacted = maybe (count r.repealed "measure" <> " were repealed.") (\who -> capitalise (listed who) <> " repealed " <> (if length r.repealed == 1 then "a measure" else show (length r.repealed) <> " measures") <> ", lifting it from the next period.") (owners r.repealed)
    | otherwise = count r.enacted "new measure" <> " came into force and " <> count r.repealed "measure" <> " were repealed."
  enactedHeadline who = capitalise (listed who) <> (if length who == 1 then " enacts " else " enact ") <> measures r.enacted

  markets = case biggestMove of
    Nothing -> []
    Just p ->
      [ { kicker: "Markets"
        , headline: moveHeadline p
        , summary: moveSummary p
        , body: map (\q -> Names.exchange q.exchange <> " at " <> Names.pricedProse q.exchange q.price <> " (" <> Names.signed q.change <> ")") r.prices
        }
      ]
  biggestMove = foldl (\best p -> if maybe true (\b -> magnitude p.change > magnitude b.change) best then Just p else best) Nothing r.prices
  moveSummary p = moveClause p <> rateClause p <> "."
  moveClause p = case direction p.change of
    Up -> "The " <> lowerFirst (Names.exchange p.exchange) <> " rose " <> unsigned p.change <> " to " <> Names.pricedProse p.exchange p.price
    Down -> "The " <> lowerFirst (Names.exchange p.exchange) <> " fell " <> unsigned p.change <> " to " <> Names.pricedProse p.exchange p.price
    Flat -> "Prices held steady"
  rateClause p = case find (\q -> Names.isRate q.exchange && q.exchange /= p.exchange) r.prices of
    Just q -> ", and the dollar fetched " <> q.price <> " renminbi"
    Nothing -> ""
  moveHeadline p = case direction p.change of
    Up -> Names.exchange p.exchange <> " rises to " <> Names.pricedProse p.exchange p.price
    Down -> Names.exchange p.exchange <> " falls to " <> Names.pricedProse p.exchange p.price
    Flat -> "Markets hold steady in round " <> show r.round

  distress
    | null r.shortfalls = []
    | otherwise =
        [ { kicker: "Shortages"
          , headline: "Shortages hit " <> listed (nub (map (_.actor >>> Names.actor) r.shortfalls))
          , summary: shortSummary
          , body: map (\x -> capitalise (Names.actor x.actor) <> " is " <> x.short <> " short of " <> Names.resource x.resource) r.shortfalls
          }
        ]

  shortSummary = case uncons (nub (map _.actor r.shortfalls)) of
    Nothing -> ""
    Just { head: first, tail: rest } ->
      capitalise (Names.actor first) <> " ran short of " <> shortOf first
        <> foldl (\acc a -> acc <> ", and " <> Names.actor a <> " of " <> shortOf a) "" rest
        <> ", leaving demand it could not meet."
  shortOf a = listed (map (_.resource >>> Names.resource) (filter (\x -> x.actor == a) r.shortfalls))

  failures
    | null r.rejected && null r.lapsed = []
    | otherwise =
        [ { kicker: "Dealmaking"
          , headline: capitalise (count (map _.candidateTransaction r.rejected <> r.lapsed) "deal") <> " fall through"
          , summary: failSummary
          , body: map (\x -> capitalise (Names.deal x.candidateTransaction) <> " was refused: " <> Names.phrase x.reason) r.rejected
              <> map lapsedLine (nub (map Names.dealOwner r.lapsed))
          }
        ]
  failSummary
    | null r.rejected = capitalise (count r.lapsed "offer") <> " lapsed unanswered, from " <> listed (nub (map Names.dealOwner r.lapsed)) <> "."
    | null r.lapsed = capitalise (count r.rejected "deal") <> (if length r.rejected == 1 then " was" else " were") <> " refused."
    | otherwise = capitalise (count r.rejected "deal") <> (if length r.rejected == 1 then " was" else " were") <> " refused and " <> count r.lapsed "offer" <> " lapsed unanswered."
  -- what moved in the balances: holdings whose change is not zero, largest first
  moved keep = sortWith (\b -> negate (magnitude b.change)) (filter (\b -> keep b && direction b.change /= Flat) r.balances)

  finance = case moved (\b -> Names.isCurrency b.resource && not (Names.isHousehold b.actor) && b.actor /= "ROW") of
    xs | Just top <- head xs ->
      [ { kicker: "Finance"
        , headline: capitalise (Names.actor top.actor) <> (if direction top.change == Up then " takes in " else " pays out ") <> Names.money top.resource (unsigned top.change)
        , summary: capitalise (Names.actor top.actor) <> (if direction top.change == Up then " took in " else " paid out ") <> Names.money top.resource (unsigned top.change) <> ", leaving it with " <> Names.money top.resource top.holding <> "." <> second xs (\b -> " " <> capitalise (Names.actor b.actor) <> " now holds " <> Names.money b.resource b.holding <> ".")
        , body: map (\b -> capitalise (Names.actor b.actor) <> " holds " <> Names.money b.resource b.holding <> " (" <> Names.signed b.change <> ")") (take 4 xs)
        }
      ]
    _ -> []

  industry = case moved (\b -> Names.isCapability b.resource) of
    xs | Just top <- head xs ->
      [ { kicker: "Industry"
        , headline: capitalise (Names.actor top.actor) <> (if direction top.change == Up then " builds up its " else " loses some of its ") <> Names.resource top.resource
        , summary: capitalise (Names.possessive top.actor) <> " " <> Names.resource top.resource <> (if direction top.change == Up && unsigned top.change == top.holding then " was built from nothing to " <> top.holding else (if direction top.change == Up then " rose " else " fell ") <> unsigned top.change <> " to " <> top.holding) <> "." <> second xs (\b -> " " <> capitalise (Names.possessive b.actor) <> " " <> Names.resource b.resource <> " stands at " <> b.holding <> ".")
        , body: map (\b -> capitalise (Names.possessive b.actor) <> " " <> Names.resource b.resource <> " at " <> b.holding <> " (" <> Names.signed b.change <> ")") (take 4 xs)
        }
      ]
    _ -> []

  households = case moved (\b -> Names.isHousehold b.actor) of
    xs | Just top <- head xs ->
      [ { kicker: "Households"
        , headline: capitalise (Names.actor top.actor) <> (if direction top.change == Up then " gain " else " lose ") <> Names.money top.resource (unsigned top.change)
        , summary: capitalise (Names.actor top.actor) <> (if direction top.change == Up then " gained " else " lost ") <> Names.money top.resource (unsigned top.change) <> ", leaving them with " <> Names.money top.resource top.holding <> "."
        , body: map (\b -> capitalise (Names.actor b.actor) <> " hold " <> Names.money b.resource b.holding <> " (" <> Names.signed b.change <> ")") (take 4 xs)
        }
      ]
    _ -> []

  lapsedLine owner = capitalise owner <> ": " <> count (filter (\x -> Names.dealOwner x == owner) r.lapsed) "offer" <> " lapsed unanswered"

-- | The second of a list, said one way; nothing when there is none.
second :: forall a. Array a -> (a -> String) -> String
second xs f = maybe "" f (index xs 1)

-- | A name as it reads mid-sentence: "World industrial market" is "world industrial market", but an acronym such as "US"
-- | keeps its capitals.
lowerFirst :: String -> String
lowerFirst s
  | String.take 2 s == toUpper (String.take 2 s) = s
  | otherwise = String.toLower (String.take 1 s) <> drop 1 s

-- | Who proposed each measure, once each, when every id names its proposer; otherwise nothing, and the paper falls
-- | back to the measures' own names.
owners :: Array String -> Maybe (Array String)
owners ids = nub <$> traverse Names.measureOwner ids

-- | "a new measure", "3 new measures".
measures :: Array String -> String
measures ids = if length ids == 1 then "a new measure" else show (length ids) <> " new measures"

-- | One line per proposer: "In force: a measure from the United States", "In force: 2 measures from China"; a measure
-- | whose id names no proposer keeps a line of its own.
measureLines :: String -> Array String -> Array String
measureLines label ids =
  map (\who -> label <> ": " <> (let n = length (filter (\i -> Names.measureOwner i == Just who) ids) in if n == 1 then "a measure" else show n <> " measures") <> " from " <> who)
    (nub (mapMaybe Names.measureOwner ids))
    <> map (\i -> label <> ": " <> Names.phrase i) (filter (isNothing <<< Names.measureOwner) ids)

-- | A change without its sign: "-4.14" is "4.14".
unsigned :: String -> String
unsigned c = fromMaybe c (stripPrefix (Pattern "-") c <|> stripPrefix (Pattern "+") c)

-- | How many stories the page prints under the lead: the first in priority order.
topStories :: Int
topStories = 4

count :: forall a. Array a -> String -> String
count xs noun = show (length xs) <> " " <> noun <> (if length xs == 1 then "" else "s")

-- | "a", "a and b", "a, b and c".
listed :: Array String -> String
listed xs = case unsnoc xs of
  Nothing -> ""
  Just { init: [], last } -> last
  Just { init, last } -> joinWith ", " init <> " and " <> last

capitalise :: String -> String
capitalise s = toUpper (String.take 1 s) <> drop 1 s

-- | How far a display-string change moved, ignoring its sign; text that is not a number (such as "<0.01") is 0.
magnitude :: String -> Number
magnitude c = abs (fromMaybe 0.0 (fromString (replaceAll (Pattern "+") (Replacement "") (replaceAll (Pattern "−") (Replacement "-") c))))
