-- | The `WIRE` interpreter: the engine's open routes over an HTTP GET device. Paths and decoding live here; the device
-- | only moves text.
module Handlers.Wire (HttpDevice, Response, interpretWire) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Edition.Report (decodeLive, decodeReport)
import Effect.Aff (Aff)
import Effects.Signatures (WireF(..))

type Response = { status :: Int, body :: String }

-- | A GET of a URL: Left when nothing answered.
type HttpDevice = { get :: String -> Aff (Either String Response) }

-- | The engine at a base URL (no trailing slash).
interpretWire :: HttpDevice -> String -> WireF ~> Aff
interpretWire http engine = case _ of
  FetchLive k -> k <$> read decodeLive "/api/live"
  FetchReport run round k -> k <$> read decodeReport ("/api/runs/" <> run <> "/rounds/" <> show round <> "/report")
  where
  read :: forall a. (Json -> Either JsonDecodeError a) -> String -> Aff (Either String (Maybe a))
  read decode path = http.get (engine <> path) <#> case _ of
    Left e -> Left (path <> ": " <> e)
    Right { status: 404 } -> Right Nothing
    Right { status: 200, body } -> case jsonParser body >>= (lmap' <<< decode) of
      Left e -> Left (path <> ": " <> e)
      Right a -> Right (Just a)
    Right { status } -> Left (path <> ": status " <> show status)

  lmap' :: forall a. Either JsonDecodeError a -> Either String a
  lmap' = case _ of
    Left e -> Left (printJsonDecodeError e)
    Right a -> Right a
