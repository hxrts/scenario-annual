-- | The `WRITE` interpreter: a writer's pass over one front page, as scenario-engine's joint pass is over a round's
-- | transcripts, but with exactly one call and no corrections, so a round costs one prompt. A reply that decodes is
-- | vetted story by story (`Edition.Brief.vet`): the stories that stand are the copy, and the rest keep the page's own
-- | headlines. A call that fails, a reply that does not decode, or one in which no story stands is a `Left`.
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
import Edition.Brief (Brief, Copy, Vetted, vet)
import Effect.Aff (Aff)
import Effects.Signatures (WriteF(..))
import Handlers.Llm.Backend (Backend, send)
import Handlers.Llm.Prompt (writePrompt)

interpretWrite :: Backend -> WriteF ~> Aff
interpretWrite b (Write brief k) = k <$> writer b brief

writer :: Backend -> Brief -> Aff (Either String Vetted)
writer b brief = send b writePrompt (stringify (encodeJson brief)) <#> \reply -> do
  text <- reply
  copy <- lmap ("the reply does not decode: " <> _) (decodeCopy text)
  let vetted = vet brief copy
  if null vetted.copy then Left ("no story in the reply stands: " <> joinWith "; " vetted.dropped) else Right vetted

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
