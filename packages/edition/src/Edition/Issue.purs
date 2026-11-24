-- | What the paper prints from: the backend reads the engine and holds one Issue, and the newspaper app reads that
-- | Issue (`GET /api/edition`). The report inside is the engine's report cut to the stable summary fields of
-- | `Edition.Report`, so `events` never reaches the app.
module Edition.Issue
  ( Issue(..)
  , encodeIssue
  , decodeIssue
  , sameRound
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Edition.Brief (Copy)
import Edition.Report (Report, decodeReport)

-- | No live run; a live run with no played round yet; or a live run's newest played round, with the copy a writer
-- | gave it once there is some (until then, and if the writer fails, the page prints its own headlines).
data Issue = NoRun | Waiting String | Printed String Report (Maybe Copy)

derive instance Eq Issue

instance Show Issue where
  show = case _ of
    NoRun -> "NoRun"
    Waiting run -> "(Waiting " <> show run <> ")"
    Printed run r copy -> "(Printed " <> show run <> " round " <> show r.round
      <>
        ( case copy of
            Just _ -> ", written"
            Nothing -> ""
        )
      <> ")"

-- | `{"status":"none"}`, `{"status":"waiting","run":…}` or `{"status":"printed","run":…,"report":…,"copy":…}`, the copy
-- | absent until it is written.
encodeIssue :: Issue -> Json
encodeIssue = case _ of
  NoRun -> encodeJson { status: "none" }
  Waiting run -> encodeJson { status: "waiting", run }
  Printed run report copy -> encodeJson { status: "printed", run, report, copy }

decodeIssue :: Json -> Either JsonDecodeError Issue
decodeIssue json = do
  o <- decodeJson json
  status <- o .: "status"
  case status of
    "none" -> pure NoRun
    "waiting" -> Waiting <$> o .: "run"
    "printed" -> Printed <$> o .: "run" <*> (decodeReport =<< o .: "report") <*> o .:? "copy"
    other -> Left (TypeMismatch ("an issue status, not " <> show other))

-- | Whether two issues print the same round of the same run, whatever copy each has.
sameRound :: Issue -> Issue -> Boolean
sameRound (Printed a r _) (Printed b q _) = a == b && r == q
sameRound a b = a == b
