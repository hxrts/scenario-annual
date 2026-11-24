-- | The `ARCHIVE` interpreter: the record of every round given to the writer, held in a reference and written whole to
-- | one file on every change, so it outlives the process. The device moves only text.
module Handlers.Archive (TextFile, Archive, openArchive, interpretArchive) where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (JsonDecodeError(..), decodeJson, printJsonDecodeError, (.:))
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effects.Signatures (ArchiveF(..), Entry(..))
import Foreign.Object (Object)
import Foreign.Object as Object

-- | One text file: its contents when it exists, and replacing them.
type TextFile = { read :: Effect (Maybe String), write :: String -> Effect Unit }

newtype Archive = Archive { entries :: Ref (Object Entry), file :: TextFile }

-- | The archive in a file: empty when the file does not exist yet. A file that exists but does not read is Left, so
-- | the backend stops rather than forget what it has asked and ask again.
openArchive :: TextFile -> Effect (Either String Archive)
openArchive file = file.read >>= case _ of
  Nothing -> Right <$> start Object.empty
  Just text -> case jsonParser text >>= (lmap printJsonDecodeError <<< decodeEntries) of
    Left e -> pure (Left ("the archive does not read: " <> e))
    Right entries -> Right <$> start entries
  where
  start entries = (\ref -> Archive { entries: ref, file }) <$> Ref.new entries

interpretArchive :: forall m. MonadEffect m => Archive -> ArchiveF ~> m
interpretArchive (Archive a) = liftEffect <<< case _ of
  Recall key k -> k <<< Object.lookup key <$> Ref.read a.entries
  Keep key entry next -> do
    entries <- Ref.modify (Object.insert key entry) a.entries
    next <$ a.file.write (stringify (encodeJson (map encodeEntry entries)))

encodeEntry :: Entry -> Json
encodeEntry = case _ of
  Asked -> encodeJson { status: "asked" }
  Written copy -> encodeJson { status: "written", copy }
  Failed reason -> encodeJson { status: "failed", reason }

decodeEntries :: Json -> Either JsonDecodeError (Object Entry)
decodeEntries json = decodeJson json >>= traverse decodeEntry
  where
  decodeEntry :: Json -> Either JsonDecodeError Entry
  decodeEntry j = do
    o <- decodeJson j
    status <- o .: "status"
    case status of
      "asked" -> pure Asked
      "written" -> Written <$> o .: "copy"
      "failed" -> Failed <$> o .: "reason"
      other -> Left (TypeMismatch ("an archive status, not " <> show other))
