-- | The backend's one route for readers, as a program over `DESK`: `GET /api/edition` answers the held issue.
-- |
-- | Caching: every reply carries an entity tag, the fingerprint of its body, and `Cache-Control: no-cache`, so a
-- | browser or proxy may keep a copy but must ask before using it. A request whose `If-None-Match` names the current
-- | tag is answered 304 with no body; a new issue has a new body, so a new tag, and every copy is invalid at once.
module Effects.Api (Request, Reply, answer) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Array (any)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.String (Pattern(..), split, stripPrefix, trim)
import Edition.Digest (digest)
import Edition.Issue (encodeIssue)
import Effects.Edition (current)
import Effects.Signatures (DESK)
import Run (Run)
import Type.Row (type (+))

type Request = { method :: String, path :: String, header :: String -> Maybe String }

type Reply = { status :: Int, body :: String, headers :: Array { name :: String, value :: String } }

answer :: forall r. Request -> Run (DESK + r) Reply
answer req
  | req.method /= "GET" && req.method /= "HEAD" = pure (failure 405 "method not allowed")
  | req.path == "/api/edition" = current <#> \issue ->
      let
        body = stringify (encodeIssue issue)
        tag = "\"" <> digest body <> "\""
        headers = [ { name: "ETag", value: tag }, { name: "Cache-Control", value: "no-cache" } ]
      in
        if maybe false (matches tag) (req.header "if-none-match") then { status: 304, body: "", headers }
        else { status: 200, body, headers }
  | otherwise = pure (failure 404 "not found")

failure :: Int -> String -> Reply
failure status error = { status, body: stringify' error, headers: [ { name: "Cache-Control", value: "no-store" } ] }
  where
  stringify' e = "{\"error\":\"" <> e <> "\"}"

-- | `If-None-Match` is a list of tags or `*`; a proxy that compresses the body may weaken a tag (`W/"…"`), which
-- | still names the same content.
matches :: String -> String -> Boolean
matches tag header = any same (split (Pattern ",") header)
  where
  same t = let t' = trim t in t' == "*" || fromMaybe t' (stripPrefix (Pattern "W/") t') == tag
