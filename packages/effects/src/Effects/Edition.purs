-- | The backend's programs, over its rows: gathering an issue from the engine, refreshing the held issue, and reading
-- | it for a reader.
module Effects.Edition
  ( fetchLive
  , fetchReport
  , current
  , publish
  , note
  , gather
  , latest
  , write
  , recall
  , keep
  , refresh
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Array (null)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Edition.Brief (Brief, Copy, Vetted, brief)
import Edition.FrontPage (frontPage)
import Edition.Issue (Issue(..), sameRound)
import Edition.Report (Live, Report, playedThrough)
import Effects.Signatures (ARCHIVE, ArchiveF(..), DESK, DeskF(..), Entry(..), LOG, LogF(..), WIRE, WireF(..), WRITE, WriteF(..), _archive, _desk, _log, _wire, _write)
import Run (Run, lift)
import Type.Row (type (+))

fetchLive :: forall r. Run (WIRE + r) (Either String (Maybe Live))
fetchLive = lift _wire (FetchLive identity)

fetchReport :: forall r. String -> Int -> Run (WIRE + r) (Either String (Maybe Report))
fetchReport run round = lift _wire (FetchReport run round identity)

current :: forall r. Run (DESK + r) Issue
current = lift _desk (Current identity)

publish :: forall r. Issue -> Run (DESK + r) Unit
publish issue = lift _desk (Publish issue unit)

write :: forall r. Brief -> Run (WRITE + r) (Either String Vetted)
write b = lift _write (Write b identity)

recall :: forall r. String -> Run (ARCHIVE + r) (Maybe Entry)
recall key = lift _archive (Recall key identity)

keep :: forall r. String -> Entry -> Run (ARCHIVE + r) Unit
keep key entry = lift _archive (Keep key entry unit)

note :: forall r. String -> Run (LOG + r) Unit
note line = lift _log (Note line unit)

-- | The issue the engine supports now: no live run, a live run with nothing played, or its newest played round.
gather :: forall r. Run (WIRE + r) (Either String Issue)
gather = fetchLive >>= case _ of
  Left e -> pure (Left e)
  Right Nothing -> pure (Right NoRun)
  Right (Just l) -> latest l.id (playedThrough l + 1) <#> map case _ of
    Nothing -> Waiting l.id
    Just r -> Printed l.id r Nothing

-- | The newest played round's report at or below `round`. The live state's round can lag a round already played,
-- | so the round itself is tried first and a 404 falls back to the one before.
latest :: forall r. String -> Int -> Run (WIRE + r) (Either String (Maybe Report))
latest run round
  | round < 1 = pure (Right Nothing)
  | otherwise = fetchReport run round >>= case _ of
      Right Nothing -> latest run (round - 1)
      found -> pure found

-- | Gather and publish. A failure to reach the engine keeps the issue already held, so readers keep the last good
-- | paper, and is noted. A newly played round is published at once with the page's own headlines, then written, and
-- | republished with its copy.
-- |
-- | The writer is asked once per round, ever: the round goes on record as asked before the call, and the copy or the
-- | failure after it, and the record outlives the process. A round already on record is not asked again: its copy is
-- | reused, and a round that failed, or whose call was cut off by a restart, keeps the page's own headlines.
refresh :: forall r. Run (WIRE + DESK + WRITE + ARCHIVE + LOG + r) Unit
refresh = gather >>= case _ of
  Left e -> note ("engine unreachable, keeping the held issue: " <> e)
  Right issue -> do
    held <- current
    unless (sameRound issue held) do
      publish issue
      note ("published " <> show issue)
      case issue of
        Printed run r Nothing -> copyFor run r >>= case _ of
          Just copy -> do
            let written = Printed run r (Just copy)
            publish written
            note ("published " <> show written)
          Nothing -> pure unit
        _ -> pure unit

-- | One round's copy: from the record, or from the writer's one call when the round has none.
copyFor :: forall r. String -> Report -> Run (WRITE + ARCHIVE + LOG + r) (Maybe Copy)
copyFor run r = recall key >>= case _ of
  Just (Written copy) -> pure (Just copy)
  Just (Failed e) -> Nothing <$ note ("round " <> key <> " was not written, and is not asked again: " <> e)
  Just Asked -> Nothing <$ note ("round " <> key <> " was asked when the backend stopped, and is not asked again")
  Nothing -> do
    keep key Asked
    write (brief (frontPage r)) >>= case _ of
      Left e -> do
        keep key (Failed e)
        Nothing <$ note ("no copy for " <> key <> ", printing the page's own headlines: " <> e)
      Right vetted -> do
        keep key (Written vetted.copy)
        unless (null vetted.dropped) do
          note ("some stories of " <> key <> " keep the page's own headlines: " <> joinWith "; " vetted.dropped)
        pure (Just vetted.copy)
  where
  key = run <> "/" <> show r.round
