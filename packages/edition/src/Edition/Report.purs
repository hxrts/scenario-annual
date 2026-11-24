-- | The engine's round report (`GET /api/runs/:id/rounds/:round/report`, openapi/narrative.json) and the live run's
-- | state (`GET /api/live`, openapi/transcription.json). Only the stable summary fields are decoded: `events` changes
-- | with the engine's store schema and is not part of the contract we read.
module Edition.Report where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Either (Either)
import Data.Maybe (Maybe)

type Price = { exchange :: String, price :: String, change :: String }
type Balance = { actor :: String, resource :: String, holding :: String, change :: String }
type Shortfall = { actor :: String, resource :: String, short :: String }
type Rejection = { candidateTransaction :: String, reason :: String }

type Report =
  { round :: Int
  , rounds :: Int -- the run's round count
  , roundMonths :: Int -- the in-game months one round stands for, as the engine was told
  , prices :: Array Price
  , balances :: Array Balance
  , shortfalls :: Array Shortfall
  , enacted :: Array String
  , repealed :: Array String
  , rejected :: Array Rejection
  , lapsed :: Array String
  }

type Live =
  { id :: String
  , round :: Int
  , status :: String
  , paused :: Boolean
  , remaining :: Maybe Int
  }

decodeReport :: Json -> Either JsonDecodeError Report
decodeReport = decodeJson

decodeLive :: Json -> Either JsonDecodeError Live
decodeLive = decodeJson

-- | The rounds whose reports are readable (docs/07 §8.2 step 5): every round before the live one, and the live one
-- | too once the run has completed.
playedThrough :: Live -> Int
playedThrough l = if l.status == "completed" then l.round else l.round - 1
