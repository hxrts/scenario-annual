module Test.Main where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Edition.Brief (period)
import Edition.Calendar (addMonths, issueDate)
import Edition.FrontPage (Direction(..), direction, frontPage)
import Data.Either (Either(..))
import Edition.Issue (Issue(..), decodeIssue, encodeIssue)
import Edition.Report (Report, playedThrough)
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

empty :: Report
empty = { round: 1, rounds: 5, roundMonths: 24, prices: [], balances: [], shortfalls: [], enacted: [], repealed: [], rejected: [], lapsed: [] }

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "frontPage" do
    it "a quiet round has no lead" do
      (frontPage empty).lead `shouldEqual` Nothing
    it "policy leads over markets" do
      let p = frontPage empty { enacted = [ "tariff" ], prices = [ { exchange: "USD/CNY", price: "7.1", change: "+0.2" } ] }
      map _.headline p.lead `shouldEqual` Just "New rules take effect: Tariff"
      map _.headline p.stories `shouldEqual` [ "USD/CNY rises to 7.1" ]
    it "names who enacted a measure when its id says, and keeps a name it cannot read" do
      let
        one = frontPage empty { enacted = [ "r1/US/4/rule 1" ] }
        two = frontPage empty { enacted = [ "r1/US/4/rule 1", "r2/CN/2/rule 1", "r2/US/5/rule 1" ] }
      map _.headline one.lead `shouldEqual` Just "The United States enacts a new measure"
      map _.body one.lead `shouldEqual` Just [ "In force: a measure from the United States" ]
      map _.headline two.lead `shouldEqual` Just "The United States and China enact 3 new measures"
      map _.body two.lead `shouldEqual` Just [ "In force: 2 measures from the United States", "In force: a measure from China" ]
      map _.headline (frontPage empty { repealed = [ "r1/US/4/rule 1" ] }).lead `shouldEqual` Just "A measure from the United States is repealed"
    it "finance, industry and households stories read the balances, largest move first, and only what moved" do
      let
        bal actor resource holding change = { actor, resource, holding, change }
        p = frontPage empty
          { balances =
              [ bal "CN" "RMB" "96.19" "16.58"
              , bal "US" "USD" "111.23" "-4.14"
              , bal "US" "K_T" "2" "0"
              , bal "CN-firm" "K_I" "2.5" "0.5"
              , bal "US-private" "USD" "6.9" "3.46"
              , bal "CN-private" "RMB" "5.82" "-1.29"
              , bal "ROW" "USD" "20.43" "-1.57"
              ]
          }
      map _.kicker (maybe [] pure p.lead <> p.stories) `shouldEqual` [ "Finance", "Industry", "Households" ]
      map _.headline (maybe [] pure p.lead <> p.stories) `shouldEqual`
        [ "China takes in ¥16.58", "The Chinese firm builds up its industrial capacity", "US households gain $3.46" ]
      map _.body p.lead `shouldEqual` Just [ "China holds ¥96.19 (+16.58)", "The United States holds $111.23 (−4.14)" ]
      map _.body p.stories `shouldEqual`
        [ [ "The Chinese firm's industrial capacity at 2.5 (+0.5)" ]
        , [ "US households hold $6.9 (+3.46)", "Chinese households hold ¥5.82 (−1.29)" ]
        ]
    it "prints the lead and at most four top stories, the first in priority order" do
      let
        p = frontPage empty
          { enacted = [ "r1/US/4/rule 1" ]
          , prices = [ { exchange: "industry-cn", price: "2.55", change: "0.03" } ]
          , shortfalls = [ { actor: "US", resource: "T", short: "1" } ]
          , balances = [ { actor: "CN", resource: "RMB", holding: "9", change: "1" }, { actor: "CN-firm", resource: "K_I", holding: "2", change: "1" }, { actor: "US-private", resource: "USD", holding: "3", change: "1" } ]
          , lapsed = [ "r1/CN/1" ]
          }
      map _.kicker p.lead `shouldEqual` Just "Policy"
      map _.kicker p.stories `shouldEqual` [ "Markets", "Shortages", "Finance", "Industry" ]
    it "orders the strip: world tech, world industry, then the US, then China, then the rest" do
      let
        at x = { exchange: x, price: "1", change: "0" }
        p = frontPage empty { prices = map at [ "usd-rmb", "industry-cn", "technology-world", "technology-us", "industry-world" ] }
      map _.name p.ticker `shouldEqual` [ "World tech", "World industry", "US tech", "China industry", "USD/CNY" ]
      map _.rate p.ticker `shouldEqual` [ false, false, false, false, true ]
      map _.body (frontPage empty { prices = [ at "usd-rmb" ] }).lead `shouldEqual` Just [ "Dollar–renminbi rate at 1 renminbi per dollar (0)" ]
      map _.price p.ticker `shouldEqual` [ "$1", "¥1", "$1", "¥1", "1" ]
    it "names actors, goods and markets in plain English" do
      let
        p = frontPage empty
          { prices = [ { exchange: "industry-cn", price: "2.55", change: "0.03" }, { exchange: "technology-us", price: "1.25", change: "-0.17" } ]
          , shortfalls = [ { actor: "US", resource: "T", short: "4.37" }, { actor: "CN", resource: "I", short: "2" } ]
          , rejected = [ { candidateTransaction: "r1/CN/4", reason: "cn-technology-investment order above its maximum" } ]
          , lapsed = [ "r2/CN-private/3", "r2/CN-private/4", "r2/US-firm/1" ]
          }
      map _.headline p.lead `shouldEqual` Just "US technology market falls to $1.25"
      map _.name p.ticker `shouldEqual` [ "US tech", "China industry" ]
      map _.change p.ticker `shouldEqual` [ "−0.17", "+0.03" ]
      map _.headline p.stories `shouldEqual` [ "Shortages hit the United States and China", "4 deals fall through" ]
      map _.body p.stories `shouldEqual`
        [ [ "The United States is 4.37 short of technology", "China is 2 short of industrial inputs" ]
        , [ "A deal from China was refused: China's technology investment order above its maximum"
          , "Chinese households: 2 offers lapsed unanswered"
          , "The US firm: 1 offer lapsed unanswered"
          ]
        ]
  describe "direction" do
    it "reads the sign of a display change" do
      map direction [ "+0.2", "-1.5%", "\x2212 3", "0.00", "" ] `shouldEqual` [ Up, Down, Down, Flat, Flat ]
  describe "calendar" do
    let start = { year: 2026, month: 11, day: 24 }
    it "round 1 is the start date, and each round is the months the engine names on" do
      map (issueDate 24 start) [ 0, 1, 2, 3, 6 ] `shouldEqual`
        [ start, start, { year: 2028, month: 11, day: 24 }, { year: 2030, month: 11, day: 24 }, { year: 2036, month: 11, day: 24 } ]
    it "a quarter a round moves three months a round" do
      issueDate 3 start 3 `shouldEqual` { year: 2027, month: 5, day: 24 }
    it "a day the month lacks becomes the month's last" do
      addMonths 1 { year: 2027, month: 1, day: 31 } `shouldEqual` { year: 2027, month: 2, day: 28 }
      addMonths 1 { year: 2028, month: 1, day: 31 } `shouldEqual` { year: 2028, month: 2, day: 29 }
  describe "playedThrough" do
    it "a running run has played the rounds before it" do
      playedThrough { id: "r", round: 3, status: "running", paused: false, remaining: Nothing } `shouldEqual` 2
    it "a completed run has played its last round" do
      playedThrough { id: "r", round: 3, status: "completed", paused: false, remaining: Nothing } `shouldEqual` 3
  describe "issue" do
    it "reads back what the backend writes" do
      let r = empty { round = 2, enacted = [ "tariff" ], prices = [ { exchange: "usd-rmb", price: "7.1", change: "+0.2" } ] }
      let copy = [ { slot: 0, headline: "Washington imposes a tariff", summary: "The United States put a tariff into force." } ]
      map (decodeIssue <<< encodeIssue) [ NoRun, Waiting "run-1", Printed "run-1" r Nothing, Printed "run-1" r (Just copy) ]
        `shouldEqual` [ Right NoRun, Right (Waiting "run-1"), Right (Printed "run-1" r Nothing), Right (Printed "run-1" r (Just copy)) ]
    it "a round's period in words" do
      map period [ 1, 6, 12, 18, 24, 36 ] `shouldEqual` [ "a month", "six months", "a year", "eighteen months", "two years", "three years" ]
  describe "plain summaries" do
    let
      bal actor resource holding change = { actor, resource, holding, change }
      page = frontPage empty
        { enacted = [ "r1/US/4/rule 1" ]
        , prices = [ { exchange: "industry-world", price: "2.18", change: "0.12" }, { exchange: "usd-rmb", price: "0.99", change: "-0.01" } ]
        , shortfalls = [ { actor: "US", resource: "T", short: "5" }, { actor: "US", resource: "I", short: "3.94" }, { actor: "CN", resource: "T", short: "3" } ]
        , balances = [ bal "CN" "RMB" "83.63" "-8.9", bal "US" "RMB" "19.6" "7.99", bal "US" "K_I" "2" "2" ]
        }
    it "each story reads as one plain sentence or two, until a writer's copy arrives" do
      map _.summary page.lead `shouldEqual` Just "The United States brought a new measure into force; it holds until repealed."
      map _.summary page.stories `shouldEqual`
        [ "The world industrial market rose 0.12 to ¥2.18, and the dollar fetched 0.99 renminbi."
        , "The United States ran short of technology and industrial inputs, and China of technology, leaving demand it could not meet."
        , "China paid out ¥8.9, leaving it with ¥83.63. The United States now holds ¥19.6."
        , "The United States' industrial capacity was built from nothing to 2."
        ]
    it "an acronym keeps its capitals mid-sentence" do
      map _.summary (frontPage empty { prices = [ { exchange: "technology-us", price: "1.5", change: "0.03" } ] }).lead
        `shouldEqual` Just "The US technology market rose 0.03 to $1.5."
