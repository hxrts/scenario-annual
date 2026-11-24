-- | The backend's programs through their real handlers, with a stub engine in place of the network and a stub model in
-- | place of `pi`.
module Test.Handlers.Main (main) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (find, uncons)
import Data.String (joinWith)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Edition.Issue (Issue(..), encodeIssue)
import Edition.Report (Report)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effects.Api (Reply, answer)
import Effects.Edition (refresh)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import Edition.Brief (Copy, applyCopy, brief, check, numbers, vet)
import Edition.FrontPage (frontPage)
import Effects.Signatures (_archive, _desk, _log, _wire, _write)
import Handlers.Archive (Archive, TextFile, interpretArchive, openArchive)
import Handlers.Llm.Backend (backend)
import Handlers.Llm.Write (decodeCopy, interpretWrite)
import Handlers.Desk (Desk, interpretDesk, newDesk)
import Handlers.Log (interpretLog)
import Handlers.Wire (HttpDevice, interpretWire)
import Run (case_, on, run)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

engine :: String
engine = "https://engine.test"

report :: Int -> Report
report round = { round, rounds: 5, roundMonths: 24, prices: [], balances: [], shortfalls: [], enacted: [ "tariff" ], repealed: [], rejected: [], lapsed: [] }

-- | An engine that answers the listed paths and 404s the rest; Nothing is an engine that does not answer.
stub :: Maybe (Array (Tuple String String)) -> HttpDevice
stub = case _ of
  Nothing -> { get: \_ -> pure (Left "connection refused") }
  Just routes -> { get: \url -> pure (Right (maybe404 (find (\r -> engine <> fst r == url) routes))) }
  where
  maybe404 = case _ of
    Just r -> { status: 200, body: snd r }
    Nothing -> { status: 404, body: "" }

live :: Int -> String -> Tuple String String
live round status = Tuple "/api/live" (stringify (encodeJson { id: "run-1", round, status, paused: false, remaining: (Nothing :: Maybe Int) }))

-- | The engine's report carries `events`; the backend must read past it.
played :: Int -> Tuple String String
played round = Tuple ("/api/runs/run-1/rounds/" <> show round <> "/report")
  (stringify (encodeJson { round, rounds: 5, roundMonths: 24, prices: [] :: Array Int, balances: [] :: Array Int, shortfalls: [] :: Array Int, enacted: [ "tariff" ], repealed: [] :: Array String, rejected: [] :: Array Int, lapsed: [] :: Array String, events: [ "anything" ] }))

-- | A model that gives these replies in turn, and fails once they run out; with the calls it was asked.
type Model = { replies :: Ref.Ref (Array String), calls :: Ref.Ref Int }

model :: Array String -> Aff Model
model replies = liftEffect ({ replies: _, calls: _ } <$> Ref.new replies <*> Ref.new 0)

refreshOn :: Desk -> HttpDevice -> Model -> Aff (Array String)
refreshOn desk http m = liftEffect (Ref.new Nothing) >>= \f -> archiveIn f >>= \a -> refreshWith a desk http m

-- | A text file in memory.
memory :: Ref.Ref (Maybe String) -> TextFile
memory ref = { read: Ref.read ref, write: \t -> Ref.write (Just t) ref }

archiveIn :: Ref.Ref (Maybe String) -> Aff Archive
archiveIn f = liftEffect (openArchive (memory f)) >>= case _ of
  Right a -> pure a
  Left e -> throwError (error e)

refreshWith :: Archive -> Desk -> HttpDevice -> Model -> Aff (Array String)
refreshWith archive desk http m = do
  lines <- liftEffect (Ref.new [])
  let
    call _ _ = do
      liftEffect (Ref.modify_ (_ + 1) m.calls)
      next <- liftEffect (Ref.read m.replies)
      case uncons next of
        Just { head, tail } -> liftEffect (Ref.write tail m.replies) $> { text: head }
        Nothing -> throwError (error "no reply")
  run
    ( case_
        # on _wire (interpretWire http engine)
        # on _desk (interpretDesk desk)
        # on _write (interpretWrite (backend { model: "stub", call }))
        # on _archive (interpretArchive archive)
        # on _log (interpretLog \l -> Ref.modify_ (_ <> [ l ]) lines)
    )
    refresh
  liftEffect (Ref.read lines)

-- | No model at all: every call fails.
silent :: Aff Model
silent = model []

-- | A lead within its targets: an 8-word headline and a 33-word summary, with no figure.
goodLead :: String
goodLead = reply "Washington imposes a tariff as trade tensions sharpen" "The United States put a new tariff into force, raising the cost of Chinese inputs for American buyers and signalling that Washington is prepared to trade cheaper supplies for leverage over its rival's industry."

reply :: String -> String -> String
reply headline summary = "Here is the copy:\n```json\n" <> stringify (encodeJson { stories: [ { slot: 0, headline, summary } ] }) <> "\n```"

copyOf :: String -> Copy
copyOf text = case decodeCopy text of
  Right c -> c
  Left _ -> []

ask :: Desk -> String -> String -> Maybe String -> Aff Reply
ask desk method path ifNoneMatch = run (case_ # on _desk (interpretDesk desk))
  (answer { method, path, header: \h -> if h == "if-none-match" then ifNoneMatch else Nothing })

held :: Desk -> Aff { status :: Int, body :: String }
held desk = ask desk "GET" "/api/edition" Nothing <#> \r -> { status: r.status, body: r.body }

etag :: Reply -> Maybe String
etag r = _.value <$> find (\h -> h.name == "ETag") r.headers

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "refresh" do
    it "no live run is an issue with no run" do
      desk <- liftEffect newDesk
      _ <- (silent >>= refreshOn desk (stub (Just [])))
      held desk >>= shouldEqual { status: 200, body: stringify (encodeIssue NoRun) }
    it "a running run prints its newest played round, falling back past a round not yet reported" do
      desk <- liftEffect newDesk
      notes <- silent >>= refreshOn desk (stub (Just [ live 3 "running", played 1, played 2 ]))
      held desk >>= shouldEqual { status: 200, body: stringify (encodeIssue (Printed "run-1" (report 2) Nothing)) }
      notes `shouldEqual`
        [ "published (Printed \"run-1\" round 2)"
        , "no copy for run-1/2, printing the page's own headlines: the model call failed (stub): no reply"
        ]
    it "a live run with nothing played is waiting" do
      desk <- liftEffect newDesk
      _ <- silent >>= refreshOn desk (stub (Just [ live 1 "running" ]))
      held desk >>= shouldEqual { status: 200, body: stringify (encodeIssue (Waiting "run-1")) }
    it "an unreachable engine keeps the held issue, and an unchanged issue is not republished" do
      desk <- liftEffect newDesk
      _ <- silent >>= refreshOn desk (stub (Just [ live 3 "completed", played 3 ]))
      again <- silent >>= refreshOn desk (stub (Just [ live 3 "completed", played 3 ]))
      down <- silent >>= refreshOn desk (stub Nothing)
      held desk >>= shouldEqual { status: 200, body: stringify (encodeIssue (Printed "run-1" (report 3) Nothing)) }
      again `shouldEqual` []
      down `shouldEqual` [ "engine unreachable, keeping the held issue: /api/live: connection refused" ]
  describe "answer" do
    it "only GET /api/edition is a route" do
      desk <- liftEffect newDesk
      replies <- traverse (\(Tuple m p) -> ask desk m p Nothing)
        [ Tuple "GET" "/api/edition", Tuple "GET" "/api/live", Tuple "POST" "/api/edition" ]
      map _.status replies `shouldEqual` [ 200, 404, 405 ]
    it "a copy that names the current tag is still good: 304 with no body, even weakened by a proxy" do
      desk <- liftEffect newDesk
      _ <- silent >>= refreshOn desk (stub (Just [ live 3 "running", played 2 ]))
      first <- ask desk "GET" "/api/edition" Nothing
      let tag = fromMaybe "" (etag first)
      again <- ask desk "GET" "/api/edition" (Just tag)
      weak <- ask desk "GET" "/api/edition" (Just ("\"other\", W/" <> tag))
      map (\r -> Tuple r.status r.body) [ again, weak ] `shouldEqual` [ Tuple 304 "", Tuple 304 "" ]
      etag again `shouldEqual` Just tag
    it "a new issue has a new tag, so a cached copy is answered in full" do
      desk <- liftEffect newDesk
      _ <- silent >>= refreshOn desk (stub (Just [ live 3 "running", played 2 ]))
      old <- fromMaybe "" <<< etag <$> ask desk "GET" "/api/edition" Nothing
      _ <- silent >>= refreshOn desk (stub (Just [ live 3 "completed", played 2, played 3 ]))
      fresh <- ask desk "GET" "/api/edition" (Just old)
      fresh.status `shouldEqual` 200
      (etag fresh /= Just old) `shouldEqual` true
  describe "the writer" do
    it "a new round is written once, and the paper prints the copy" do
      desk <- liftEffect newDesk
      m <- model [ goodLead ]
      notes <- refreshOn desk (stub (Just [ live 3 "running", played 2 ])) m
      held desk >>= shouldEqual { status: 200, body: stringify (encodeIssue (Printed "run-1" (report 2) (Just (copyOf goodLead)))) }
      notes `shouldEqual` [ "published (Printed \"run-1\" round 2)", "published (Printed \"run-1\" round 2, written)" ]
      _ <- refreshOn desk (stub (Just [ live 3 "running", played 2 ])) m
      liftEffect (Ref.read m.calls) >>= shouldEqual 1
    it "one prompt per round: a reply that does not stand is not asked again, and the page keeps its headlines" do
      desk <- liftEffect newDesk
      let invented = reply "Washington imposes a tariff as trade tensions sharpen" "The United States put a 25 per cent tariff into force, raising the cost of Chinese inputs for American buyers and signalling that Washington will trade cheaper supplies for leverage over its rival."
      m <- model [ invented, goodLead ]
      notes <- refreshOn desk (stub (Just [ live 3 "running", played 2 ])) m
      liftEffect (Ref.read m.calls) >>= shouldEqual 1
      held desk >>= shouldEqual { status: 200, body: stringify (encodeIssue (Printed "run-1" (report 2) Nothing)) }
      notes `shouldEqual`
        [ "published (Printed \"run-1\" round 2)"
        , "no copy for run-1/2, printing the page's own headlines: no story in the reply stands: slot 0 states 25, which neither its facts nor the markets do"
        ]
    it "the record outlives a restart: a written round reuses its copy, and a failed or cut-off round is not asked again" do
      file <- liftEffect (Ref.new Nothing)
      first <- archiveIn file
      m <- model [ goodLead ]
      _ <- liftEffect newDesk >>= \d -> refreshWith first d (stub (Just [ live 3 "running", played 2 ])) m
      -- a new process: a new desk, the same file
      again <- archiveIn file
      desk <- liftEffect newDesk
      _ <- refreshWith again desk (stub (Just [ live 3 "running", played 2 ])) m
      liftEffect (Ref.read m.calls) >>= shouldEqual 1
      held desk >>= shouldEqual { status: 200, body: stringify (encodeIssue (Printed "run-1" (report 2) (Just (copyOf goodLead)))) }
      -- a round whose call was out when the process stopped
      liftEffect (Ref.write (Just "{\"run-1/3\":{\"status\":\"asked\"}}") file)
      cut <- archiveIn file
      notes <- liftEffect newDesk >>= \d -> refreshWith cut d (stub (Just [ live 3 "completed", played 3 ])) m
      liftEffect (Ref.read m.calls) >>= shouldEqual 1
      notes `shouldEqual` [ "published (Printed \"run-1\" round 3)", "round run-1/3 was asked when the backend stopped, and is not asked again" ]
    it "an archive that does not read stops the backend rather than ask again" do
      file <- liftEffect (Ref.new (Just "not json"))
      opened <- liftEffect (openArchive (memory file))
      ( case opened of
          Left _ -> true
          Right _ -> false
      ) `shouldEqual` true
  describe "the brief" do
    it "bundles each printed story in page order, and the copy replaces its headline and summary" do
      let page = frontPage (report 2)
      map _.slot (brief page).bundles `shouldEqual` [ 0 ]
      map _.facts (brief page).bundles `shouldEqual` [ [ "In force: Tariff" ] ]
      map (\s -> joinWith "|" [ s.headline, s.summary ]) (applyCopy (copyOf goodLead) page).lead
        `shouldEqual` Just "Washington imposes a tariff as trade tensions sharpen|The United States put a new tariff into force, raising the cost of Chinese inputs for American buyers and signalling that Washington is prepared to trade cheaper supplies for leverage over its rival's industry."
    it "checks every slot answered once, the lengths, and that each figure is in the facts" do
      let b = { round: 2, rounds: 5, period: "two years", markets: [], bundles: [ { slot: 0, section: "Markets", draft: "World industrial market rises to ¥2.18", facts: [ "World industrial market at ¥2.18 (+0.12)" ] } ] }
      check b (copyOf goodLead) `shouldEqual` []
      check b [] `shouldEqual` [ "slot 0 has no story" ]
      check b (copyOf (reply "Industrial prices climb to 2.18 as buyers chase scarce supply" "World industrial prices rose by 0.12 to ¥2.18, and by 3 per cent against the dollar, as American buyers competed for scarce Chinese inputs while home-grown industry stood idle and know-how wore away.")) `shouldEqual` [ "slot 0 states 3, which neither its facts nor the markets do" ]
      numbers "¥2.18 (+0.12), up 3." `shouldEqual` [ "2.18", "0.12", "3" ]
    it "vets story by story: a figure from the market strip is allowed, and a story that fails alone keeps its placeholder" do
      let
        b =
          { round: 2
          , rounds: 5
          , period: "two years"
          , markets: [ "USD/CNY 0.99 (−0.01)" ]
          , bundles:
              [ { slot: 0, section: "Markets", draft: "World industrial market rises to ¥2.18", facts: [ "World industrial market at ¥2.18 (+0.12)" ] }
              , { slot: 1, section: "Shortages", draft: "Shortages hit China", facts: [ "China is 3 short of technology" ] }
              ]
          }
        lead = { slot: 0, headline: "Industrial prices climb as the dollar slips to 0.99", summary: "World industrial prices rose to ¥2.18 over the past two years as the dollar slipped against the renminbi, making Chinese inputs dearer for American buyers and giving idle American plants a reason to restart." }
        bad = { slot: 1, headline: "China runs 7 short of technology", summary: "China ended the period 7 units short of technology, exposed to any American export control." }
        vetted = vet b [ lead, bad, lead ]
      vetted.copy `shouldEqual` [ lead ]
      vetted.dropped `shouldEqual` [ "slot 0 is written more than once", "slot 1 states 7, which neither its facts nor the markets do" ]
