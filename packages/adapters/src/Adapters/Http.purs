-- | HTTP over Node: a GET of a URL, and a server on a port. Text in, text out.
module Adapters.Http (Request, Reply, get, listen) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Exception (message)

foreign import getImpl :: Fn2 Int String (EffectFnAff { status :: Int, body :: String })

type Request = { method :: String, path :: String, header :: String -> Maybe String }

type Reply = { status :: Int, body :: String, headers :: Array { name :: String, value :: String } }

foreign import listenImpl :: Fn3 Int String ({ method :: String, path :: String, header :: String -> Nullable String } -> Effect Reply) (Effect Unit)

-- | GET a URL, giving up after the timeout (milliseconds). Left when nothing answered.
get :: Int -> String -> Aff (Either String { status :: Int, body :: String })
get timeout url = attempt (fromEffectFnAff (runFn2 getImpl timeout url)) <#> case _ of
  Left e -> Left (message e)
  Right r -> Right r

-- | Serve on a port of a host: every request's method, path and headers (by lower-case name) go to the handler, which
-- | answers a status, a JSON body and response headers. A HEAD request gets the headers without the body.
listen :: Int -> String -> (Request -> Effect Reply) -> Effect Unit
listen port host handler = runFn3 listenImpl port host \r -> handler { method: r.method, path: r.path, header: toMaybe <<< r.header }
