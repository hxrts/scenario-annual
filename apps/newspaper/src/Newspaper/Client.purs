-- | Reads the engine's open routes (relative URLs: the dev server proxies /api to the engine).
module Newspaper.Client (live, report) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Edition.Report (Live, Report, decodeLive, decodeReport)
import Effect.Aff (Aff)
import Fetch (fetch)
import Unsafe.Coerce (unsafeCoerce)

getJson :: String -> Aff (Maybe Json)
getJson url = do
  res <- fetch url {}
  if res.status == 200 then Just <<< (unsafeCoerce :: _ -> Json) <$> res.json else pure Nothing

decoded :: forall a. (Json -> Either JsonDecodeError a) -> Maybe Json -> Either String (Maybe a)
decoded _ Nothing = Right Nothing
decoded f (Just j) = case f j of
  Left e -> Left (printJsonDecodeError e)
  Right a -> Right (Just a)

-- | The live run, or Nothing when none is live (404).
live :: Aff (Either String (Maybe Live))
live = decoded decodeLive <$> getJson "/api/live"

-- | One round's report, or Nothing until it has been played (404).
report :: String -> Int -> Aff (Either String (Maybe Report))
report run round = decoded decodeReport <$> getJson ("/api/runs/" <> run <> "/rounds/" <> show round <> "/report")
