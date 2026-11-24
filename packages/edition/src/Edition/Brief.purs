-- | What a writer is given for one front page, and what it gives back. The brief bundles, for each story the page
-- | prints (the lead first), the facts the page drew it from; the copy is a headline and a summary for each. The
-- | targets for their lengths are stated once here: the prompt states them to the writer and `check` holds the
-- | reply to them. Pure: the backend's handlers ask a model for the copy, and this module decides whether it stands.
module Edition.Brief
  ( Bundle
  , Brief
  , Written
  , Copy
  , Range
  , Targets
  , targets
  , brief
  , check
  , applyCopy
  , words
  , numbers
  , period
  ) where

import Prelude

import Data.Array (catMaybes, elem, filter, find, length, mapWithIndex, nub, null, (:))
import Data.Array as Array
import Data.Foldable (all, foldl)
import Data.Int (ceil, floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Edition.FrontPage (FrontPage, Story)

-- | One story's facts: its place on the page (0 is the lead), its section, the headline the page prints without a
-- | writer, and the facts behind it, each a sentence the page could print.
type Bundle = { slot :: Int, section :: String, draft :: String, facts :: Array String }

-- | One front page to write: which round of how many, the period a round stands for in words ("two years", "six
-- | months"), the markets as the ticker shows them, and a bundle per story.
type Brief = { round :: Int, rounds :: Int, period :: String, markets :: Array String, bundles :: Array Bundle }

-- | One story as written.
type Written = { slot :: Int, headline :: String, summary :: String }

type Copy = Array Written

-- | A length in words.
type Range = { min :: Int, max :: Int }

type Targets = { lead :: { headline :: Range, summary :: Range }, story :: { headline :: Range, summary :: Range } }

-- | The lead's headline and summary run longer than a top story's, as a paper's lead does.
targets :: Targets
targets =
  { lead: { headline: { min: 6, max: 11 }, summary: { min: 30, max: 50 } }
  , story: { headline: { min: 4, max: 9 }, summary: { min: 15, max: 30 } }
  }

brief :: FrontPage -> Brief
brief page =
  { round: page.round
  , rounds: page.rounds
  , period: period page.roundMonths
  , markets: map (\t -> t.name <> " " <> t.price <> " (" <> t.change <> ")") page.ticker
  , bundles: mapWithIndex bundle (maybe [] pure page.lead <> page.stories)
  }
  where
  bundle slot s = { slot, section: s.kicker, draft: s.headline, facts: s.body }

-- | What is wrong with a reply, each as a sentence a correction can quote; none when the copy stands. Every bundle is
-- | answered once and nothing else is; each headline and summary is within its target, give or take a quarter; and
-- | every number written is one the bundle's facts state, so a writer cannot invent a quantity.
check :: Brief -> Copy -> Array String
check b copy = missing <> extra <> twice <> Array.concatMap judged copy
  where
  slots = map _.slot b.bundles
  answered = map _.slot copy
  missing = map (\s -> "slot " <> show s <> " has no story") (filter (\s -> not (s `elem` answered)) slots)
  extra = map (\s -> "slot " <> show s <> " is not in the brief") (nub (filter (\s -> not (s `elem` slots)) answered))
  twice = map (\s -> "slot " <> show s <> " is written more than once") (nub (filter (\s -> length (filter (_ == s) answered) > 1) slots))
  judged w = case find (\x -> x.slot == w.slot) b.bundles of
    Nothing -> []
    Just bundle ->
      let
        t = if w.slot == 0 then targets.lead else targets.story
        stated = Array.concatMap numbers (bundle.draft : bundle.facts)
        invented = filter (\n -> not (n `elem` stated)) (nub (numbers w.headline <> numbers w.summary))
      in
        catMaybes
          [ length' "headline" t.headline w.headline
          , length' "summary" t.summary w.summary
          ]
          <> map (\n -> "slot " <> show w.slot <> " states " <> n <> ", which its facts do not") invented
    where
    length' part r text =
      let
        n = length (words text)
        lo = floor (toNumber r.min * 0.75)
        hi = ceil (toNumber r.max * 1.25)
      in
        if n < lo || n > hi then Just ("slot " <> show w.slot <> "'s " <> part <> " is " <> show n <> " words; write " <> show r.min <> " to " <> show r.max)
        else Nothing

-- | The page with its copy: a written story takes the writer's headline, and its summary in place of the facts.
applyCopy :: Copy -> FrontPage -> FrontPage
applyCopy copy page = page { lead = map (written 0) page.lead, stories = mapWithIndex (\i -> written (i + 1)) page.stories }
  where
  written :: Int -> Story -> Story
  written slot s = case find (\w -> w.slot == slot) copy of
    Just w -> s { headline = w.headline, body = [ w.summary ] }
    Nothing -> s

words :: String -> Array String
words = filter (_ /= "") <<< split (Pattern " ") <<< trim

-- | The numbers a text states, as written: digits with any decimal point between them ("2.56", "83.63", "5"), so
-- | "¥2.56" and "2.56," both state 2.56.
numbers :: String -> Array String
numbers text = finish (foldl step { current: [], found: [] } (toCharArray text))
  where
  digit c = c >= '0' && c <= '9'
  step acc c
    | digit c = acc { current = Array.snoc acc.current c }
    | c == '.' && not (null acc.current) && all (_ /= '.') acc.current = acc { current = Array.snoc acc.current c }
    | otherwise = close acc
  close acc = if null acc.current then acc else { current: [], found: Array.snoc acc.found (trimDot (fromCharArray acc.current)) }
  finish acc = (close acc).found
  trimDot s = case Array.unsnoc (toCharArray s) of
    Just { init, last: '.' } -> fromCharArray init
    _ -> s

-- | A span of months in words, as a reporter writes it: "six months", "a year", "eighteen months", "two years".
period :: Int -> String
period months
  | months == 12 = "a year"
  | months `mod` 12 == 0 = spelled (months / 12) <> " years"
  | months == 1 = "a month"
  | otherwise = spelled months <> " months"

spelled :: Int -> String
spelled n = fromMaybe (show n) (Array.index [ "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty" ] n)
