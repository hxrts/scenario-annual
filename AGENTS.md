# Scenario Annual

This is the newspaper component that scenario-engine names as an external service (its docs/01 and docs/07 §8.2). The engine's OpenAPI documents (`just contract` → `openapi/`) are the contract. Read only the stable summary fields of a report. Never read `events`, because it changes with the engine's store schema.

- Keep the logic in `packages/edition`, which is pure and tested. `apps/newspaper` only fetches and renders.
- Tooling matches scenario-engine: the nix flake, purs and spago pinned in package.json, `just`, and `purs-tidy`.
- Run `just check` before finishing.
