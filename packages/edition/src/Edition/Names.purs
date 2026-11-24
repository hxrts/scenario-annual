-- | Plain-English names for the engine's codes (scenario-engine docs/05_pacific.md). A code this module does not
-- | know is tidied rather than dropped: hyphens become spaces and the first letter is capitalised, so a new
-- | composition still reads.
module Edition.Names
  ( actor
  , resource
  , exchange
  , tickerName
  , tickerRank
  , isRate
  , priced
  , pricedProse
  , signed
  , deal
  , dealOwner
  , phrase
  ) where

import Prelude

import Data.Array (index)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, split, stripPrefix, take, drop, toUpper)
import Data.String.CodeUnits as CU

actor :: String -> String
actor = case _ of
  "US" -> "the United States"
  "CN" -> "China"
  "US-firm" -> "the US firm"
  "CN-firm" -> "the Chinese firm"
  "US-private" -> "US households"
  "CN-private" -> "Chinese households"
  "ROW" -> "the rest of the world"
  other -> tidy other

resource :: String -> String
resource = case _ of
  "T" -> "technology"
  "I" -> "industrial inputs"
  "USD" -> "dollars"
  "RMB" -> "renminbi"
  "K_T" -> "technology capacity"
  "K_I" -> "industrial capacity"
  "S_T" -> "technology know-how"
  "S_I" -> "industrial know-how"
  other -> tidy other

exchange :: String -> String
exchange = case _ of
  "technology-us" -> "US technology market"
  "technology-world" -> "World technology market"
  "industry-cn" -> "Chinese industrial market"
  "industry-world" -> "World industrial market"
  "usd-rmb" -> "Dollar–renminbi rate"
  other -> capitalise (tidy other)

-- | A short name for the market strip.
tickerName :: String -> String
tickerName = case _ of
  "technology-us" -> "US tech"
  "technology-world" -> "World tech"
  "industry-cn" -> "China industry"
  "industry-world" -> "World industry"
  "usd-rmb" -> "USD/CNY"
  other -> capitalise (tidy other)

-- | Where a market sits in the strip, left to right: world technology, world industry, then the US market, then
-- | China's; any other
-- | market follows, in the report's order.
tickerRank :: String -> Int
tickerRank = case _ of
  "technology-world" -> 0
  "industry-world" -> 1
  "technology-us" -> 2
  "industry-cn" -> 3
  _ -> 4

-- | Whether a market is an exchange rate between currencies rather than a market for goods.
isRate :: String -> Boolean
isRate = eq "usd-rmb"

-- | A price with its currency: technology is priced in dollars and industrial inputs in renminbi; the exchange
-- | rate is quoted the market's way, as a bare number of renminbi per dollar under its pair code (USD/CNY 1.02), and
-- | "1.02 renminbi per dollar" in prose. A market this module does not know keeps its bare number.
priced :: String -> String -> String
priced market price = case market of
  "technology-us" -> "$" <> price
  "technology-world" -> "$" <> price
  "industry-cn" -> "¥" <> price
  "industry-world" -> "¥" <> price
  _ -> price

-- | A price as a sentence reads it.
pricedProse :: String -> String -> String
pricedProse market price = priced market price <> (if isRate market then " renminbi per dollar" else "")

-- | A change with its sign: the engine writes rises unsigned.
signed :: String -> String
signed c = case CU.charAt 0 c of
  Just '-' -> "−" <> drop 1 c
  Just '+' -> c
  _ | c == "0" -> "0"
  _ -> "+" <> c

-- | Whose deal a candidate transaction id names: `r2/CN-private/3` is Chinese households'.
dealOwner :: String -> String
dealOwner id = fromMaybe id (actor <$> index (split (Pattern "/") id) 1)

-- | A candidate transaction id as a phrase: `r2/CN-private/3` is "a deal from Chinese households".
deal :: String -> String
deal id = case index (split (Pattern "/") id) 1 of
  Just a -> "a deal from " <> actor a
  Nothing -> tidy id

-- | Engine prose with codes in it, such as a rejection reason or a measure's name.
phrase :: String -> String
phrase s = capitalise (tidy (prefix s))
  where
  prefix x = case stripPrefix (Pattern "cn-") x, stripPrefix (Pattern "us-") x of
    Just rest, _ -> "China's " <> rest
    _, Just rest -> "US " <> rest
    _, _ -> x

tidy :: String -> String
tidy = replaceAll (Pattern "-") (Replacement " ") <<< replaceAll (Pattern "_") (Replacement " ")

capitalise :: String -> String
capitalise s = toUpper (take 1 s) <> drop 1 s

