-- | The `WRITE` interpreter: a writer's pass over one front page, as scenario-engine's joint pass is over a round's
-- | transcripts, but with exactly one call and no corrections, so a round costs one prompt. A reply that decodes and
-- | stands (`Edition.Brief.check`) is the copy; a call that fails, or a reply that does not stand, is a `Left` with the
-- | reason, and the paper keeps its own headlines.
module Handlers.Llm.Write (writer, interpretWrite, decodeCopy, firstObject) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.String (Pattern(..), joinWith, lastIndexOf, indexOf)
import Data.String.CodeUnits (slice)
import Data.Maybe (Maybe(..))
import Edition.Brief (Brief, Copy, check)
import Effect.Aff (Aff)
import Effects.Signatures (WriteF(..))
import Handlers.Llm.Backend (Backend, send)
import Handlers.Llm.Prompt (writePrompt)

interpretWrite :: Backend -> WriteF ~> Aff
interpretWrite b (Write brief k) = k <$> writer b brief

writer :: Backend -> Brief -> Aff (Either String Copy)
writer b brief = send b writePrompt (stringify (encodeJson brief)) <#> \reply -> do
  text <- reply
  copy <- lmap ("the reply does not decode: " <> _) (decodeCopy text)
  let wrong = check brief copy
  if null wrong then Right copy else Left ("the reply does not stand: " <> joinWith "; " wrong)

-- | A reply's copy: the first JSON object in its text (a model may wrap it in a fence or a sentence), read as
-- | `{"stories": [...]}`.
decodeCopy :: String -> Either String Copy
decodeCopy text = do
  json <- jsonParser (firstObject text)
  o :: { stories :: Copy } <- lmap printJsonDecodeError (decodeJson json)
  pure o.stories

-- | From the first `{` to the last `}`, or the text itself when it has none.
firstObject :: String -> String
firstObject text = case indexOf (Pattern "{") text, lastIndexOf (Pattern "}") text of
  Just from, Just to | to > from -> slice from (to + 1) text
  _, _ -> text
