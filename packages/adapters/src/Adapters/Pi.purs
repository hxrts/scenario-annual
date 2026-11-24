-- | The `pi` process: the raw model call, the same device as the simulator's `Adapters.Pi`. One ephemeral, tool-less `pi` process
-- | per call, given a system prompt and an input text, returning the reply text. Its raw event stream is kept under a
-- | directory, one file per call. This adapter knows nothing of prompts or JSON: those are the handler's.
module Adapters.Pi (pi) where

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import piImpl :: String -> String -> String -> String -> EffectFnAff { text :: String }

-- | One call: the system prompt and the input in, the reply text out. A missing `pi`, a non-zero exit or an
-- | unwritable directory is an Aff error, which the handler turns into a reason.
pi :: { model :: String, rawDir :: String } -> String -> String -> Aff { text :: String }
pi c system input = fromEffectFnAff (piImpl c.model c.rawDir system input)
