-- | The language-model backend, as in scenario-engine's `Handlers.Llm.Backend`: one raw call under a timeout. Model
-- | output is only a candidate; the writer's handler checks it before the paper prints it.
module Handlers.Llm.Backend
  ( Call
  , Backend
  , backend
  , defaultModel
  , defaultTimeoutMs
  , send
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Parallel (parallel, sequential)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, attempt, delay)
import Effect.Exception (message)

-- | One raw model call: the system prompt and the input in, the reply text out. A failed call is an Aff error.
type Call = String -> String -> Aff { text :: String }

-- | What a handler interprets over: the raw call, the model's name for the log, and how long a call may take.
type Backend = { model :: String, call :: Call, timeoutMs :: Int }

backend :: { model :: String, call :: Call } -> Backend
backend b = { model: b.model, call: b.call, timeoutMs: defaultTimeoutMs }

-- | The model the backend uses when none is named: the engine's own default.
defaultModel :: String
defaultModel = "openrouter/deepseek/deepseek-v4-flash-latest"

-- | How long one call may take before it counts as failed.
defaultTimeoutMs :: Int
defaultTimeoutMs = 180000

-- | One call under the backend's timeout: the reply text, or why there is none (the call failed or timed out). The
-- | backend never stops on a failed call: the paper keeps its own headlines.
send :: Backend -> String -> String -> Aff (Either String String)
send b system input = sequential (parallel (Right <$> attempt (b.call system input)) <|> parallel timer) <#> case _ of
  Right (Right out) -> Right out.text
  Right (Left e) -> Left ("the model call failed (" <> b.model <> "): " <> message e)
  Left e -> Left e
  where
  timer = delay (Milliseconds (toNumber b.timeoutMs)) $> Left ("the model call timed out after " <> show b.timeoutMs <> " ms")
