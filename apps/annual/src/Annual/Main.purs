-- | The backend's composition root: the only module that imports adapters. It closes the programs of `sa-effects`
-- | over their handlers and real devices, refreshes the held issue from the engine once a minute, and serves it to
-- | the newspaper app (`GET /api/edition`).
-- |
-- | ENGINE_URL: the engine (default https://scenario.hxrts.com). PORT: where to listen on 127.0.0.1 (default 8790).
-- | PI_MODEL: the model the writer's `pi` calls use (default: the engine's). PI_RAW_DIR: where each call's raw event
-- | stream is kept (default runs/pi). ARCHIVE: the record of the rounds given to the writer, so each is asked once
-- | across restarts (default runs/archive.json). WRITER=off prints the page's own headlines and makes no model call.
module Annual.Main (main) where

import Prelude

import Adapters.Http (get, listen)
import Adapters.File (textFile)
import Adapters.Pi (pi)
import Adapters.Process (env, writeLine)
import Data.Int (fromString, toNumber)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay, launchAff_)
import Effects.Api (answer)
import Effects.Edition (refresh)
import Effects.Signatures (WriteF(..), _archive, _desk, _log, _wire, _write)
import Effect.Exception (throw)
import Handlers.Archive (interpretArchive, openArchive)
import Handlers.Desk (newDesk, interpretDesk)
import Handlers.Llm.Backend (backend, defaultModel)
import Handlers.Llm.Write (interpretWrite)
import Handlers.Log (interpretLog)
import Handlers.Wire (interpretWire)
import Run (case_, on, run)

-- | How often the backend reads the engine, in milliseconds.
refreshMs :: Int
refreshMs = 60_000

-- | How long one engine request may take before it counts as unreachable.
timeoutMs :: Int
timeoutMs = 10_000

main :: Effect Unit
main = do
  engine <- fromMaybe "https://scenario.hxrts.com" <$> env "ENGINE_URL"
  port <- fromMaybe 8790 <<< (_ >>= fromString) <$> env "PORT"
  model <- fromMaybe defaultModel <$> env "PI_MODEL"
  rawDir <- fromMaybe "runs/pi" <$> env "PI_RAW_DIR"
  writing <- (_ /= pure "off") <$> env "WRITER"
  archivePath <- fromMaybe "runs/archive.json" <$> env "ARCHIVE"
  archive <- openArchive (textFile archivePath) >>= case _ of
    Left e -> throw (archivePath <> ": " <> e)
    Right a -> pure a
  desk <- newDesk
  let
    http = { get: get timeoutMs }
    writer =
      if writing then interpretWrite (backend { model, call: pi { model, rawDir } })
      else \(Write _ k) -> pure (k (Left "the writer is off"))
    refreshNow = run
      ( case_
          # on _wire (interpretWire http engine)
          # on _desk (interpretDesk desk)
          # on _write writer
          # on _archive (interpretArchive archive)
          # on _log (interpretLog writeLine)
      )
      refresh
    reply req = run (case_ # on _desk (interpretDesk desk)) (answer req)
  listen port "127.0.0.1" reply
  writeLine ("serving the edition on 127.0.0.1:" <> show port <> ", reading " <> engine <> (if writing then ", writing with " <> model else ", the writer off"))
  -- one refresh at a time: a slow writer delays the next read rather than overlapping it
  launchAff_ $ forever (refreshNow *> delay (Milliseconds (toNumber refreshMs)))
