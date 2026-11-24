-- | The impure vocabulary of the backend: the effect rows and their functor types. Signatures only; nothing here
-- | implements an effect. Handlers in `sa-handlers` interpret them over devices, and adapters supply the devices.
module Effects.Signatures
  ( WIRE
  , WireF(..)
  , _wire
  , DESK
  , DeskF(..)
  , _desk
  , LOG
  , LogF(..)
  , _log
  , WRITE
  , WriteF(..)
  , _write
  , ARCHIVE
  , ArchiveF(..)
  , Entry(..)
  , _archive
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Edition.Brief (Brief, Copy)
import Edition.Issue (Issue)
import Edition.Report (Live, Report)
import Type.Proxy (Proxy(..))

-- | The engine's open routes: the live run, and one round's report. Nothing is a 404 (no live run; a round not yet
-- | played); Left is a failure to reach or read the engine.
data WireF a
  = FetchLive (Either String (Maybe Live) -> a)
  | FetchReport String Int (Either String (Maybe Report) -> a)

derive instance Functor WireF

type WIRE r = (wire :: WireF | r)

_wire :: Proxy "wire"
_wire = Proxy

-- | The issue the backend holds for readers: the current one, and replacing it.
data DeskF a
  = Current (Issue -> a)
  | Publish Issue a

derive instance Functor DeskF

type DESK r = (desk :: DeskF | r)

_desk :: Proxy "desk"
_desk = Proxy

-- | One line for the operator.
data LogF a = Note String a

derive instance Functor LogF

type LOG r = (log :: LogF | r)

_log :: Proxy "log"
_log = Proxy

-- | A writer's pass over one front page: one model call, a headline and a summary for each story the brief bundles. Left
-- | is why no copy could be had (the call failed, or its reply does not stand).
data WriteF a = Write Brief (Either String Copy -> a)

derive instance Functor WriteF

type WRITE r = (write :: WriteF | r)

_write :: Proxy "write"
_write = Proxy

-- | What is on record for one round's copy: asked (the call was made and has not answered, or the process stopped
-- | while it was out), written, or failed with the reason. A round on record is never asked again.
data Entry = Asked | Written Copy | Failed String

derive instance Eq Entry

-- | The record of every round given to the writer, by key (`run/round`), kept across restarts.
data ArchiveF a
  = Recall String (Maybe Entry -> a)
  | Keep String Entry a

derive instance Functor ArchiveF

type ARCHIVE r = (archive :: ArchiveF | r)

_archive :: Proxy "archive"
_archive = Proxy
