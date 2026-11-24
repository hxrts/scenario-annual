-- | Reads the paper's own backend (`apps/annual`), which reads the engine. Relative URL: in development the dev
-- | server proxies /api to the backend, and in production nginx does.
module Newspaper.Client (edition) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Either (Either(..))
import Edition.Issue (Issue, decodeIssue)
import Effect.Aff (Aff, attempt)
import Effect.Exception (message)
import Fetch (fetch)
import Unsafe.Coerce (unsafeCoerce)

-- | The issue the backend holds. Left when the backend cannot be reached or read.
edition :: Aff (Either String Issue)
edition = attempt (fetch "/api/edition" {}) >>= case _ of
  Left e -> pure (Left (message e))
  Right res
    | res.status /= 200 -> pure (Left ("the edition answered " <> show res.status))
    | otherwise -> res.json <#> \j -> case decodeIssue ((unsafeCoerce :: _ -> Json) j) of
        Left e -> Left (printJsonDecodeError e)
        Right issue -> Right issue
