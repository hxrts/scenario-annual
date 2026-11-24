# Scenario Annual

This is the newspaper component that scenario-engine names as an external service (its docs/01 and docs/07 §8.2). The engine's OpenAPI documents (`just contract` → `openapi/`) are the contract. Read only the stable summary fields of a report. Never read `events`, because it changes with the engine's store schema.

- Keep the logic in `packages/edition`, which is pure and tested. `apps/newspaper` only fetches and renders.
- The backend (`apps/annual`) reads the engine once a minute and serves the held issue at `GET /api/edition`; the newspaper reads only that, once a minute. It follows scenario-engine's layers for impure work (its docs/08 §1):
  - `packages/effects` declares the rows (`WIRE` the engine, `DESK` the held issue, `LOG`) and the programs over them (`Effects.Edition.refresh`, `Effects.Api.answer`). No implementation, no `Effect` or `Aff`.
  - `packages/handlers` interprets the rows over device records (`Handlers.Wire`, `Handlers.Desk`, `Handlers.Log`). Paths and decoding live here. It never imports an adapter; its tests run the programs over stub devices.
  - `packages/adapters` holds the raw Node devices (HTTP get and listen, environment, stdout, timer). It knows nothing of the paper.
  - `apps/annual/src/Annual/Main.purs` is the only composition root: the only module that imports adapters.
  - The writer: when a new round is printed, `refresh` builds a brief (`Edition.Brief.brief`: one bundle of facts per story the front page prints, lead first) and asks the `WRITE` row for a headline and summary per story. `Handlers.Llm.Write` makes one `pi` call (`Adapters.Pi`) with the system prompt in `Handlers.Llm.Prompt` (the world, the economics, how to read the brief, the voice, the lengths from `Edition.Brief.targets`), and `Edition.Brief.check` decides whether the reply stands: every story answered once, lengths in range, no figure its facts do not state. One prompt per round, ever: the `ARCHIVE` row (`Handlers.Archive`, a JSON file, `ARCHIVE`, default `runs/archive.json`) records the round as asked before the call and the copy or failure after it, so a refresh or restart reuses written copy and never asks again. A failed or rejected round keeps the page's own headlines. `WRITER=off` disables the writer; `PI_MODEL` names the model.
  - `scripts/check/layers.js` (part of `just check`) enforces these rules and the dependency direction between packages.
- Tooling matches scenario-engine: the nix flake, purs and spago pinned in package.json, `just`, and `purs-tidy`.
- Run `just check` before finishing.
