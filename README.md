# Scenario Annual

The newspaper for [Scenario Engine](../scenario-engine). It follows the engine's live run and prints a front page for each round it plays.

It uses only the engine's open routes (scenario-engine `docs/07_simulator.md` §8.2):

- `GET /api/live`: the live run's id, round and status
- `GET /api/runs/:id/rounds/:round/report`: one played round's summary

## Layout

- `packages/edition` (`sa-edition`): pure code. It decodes the report and live state (`Edition.Report`) and composes a front page (`Edition.FrontPage`).
- `apps/newspaper` (`sa-newspaper`): the react-basic-hooks app, bundled by vite.

## Use

```sh
nix develop        # or direnv: node 22 + just, with purs and spago pinned through npm
just               # list tasks
just check         # fmt-check, build, test
just dev           # http://localhost:5180, proxying /api to ENGINE_URL (default https://scenario.hxrts.com)
just contract      # copy the engine's OpenAPI documents into openapi/
```

To use a local engine instead, run `just serve` in scenario-engine and `ENGINE_URL=http://localhost:8787 just dev` here.
