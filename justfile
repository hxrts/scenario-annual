export PATH := justfile_directory() + "/node_modules/.bin:" + env_var('PATH')

set tempdir := "/tmp"
export TMPDIR := "/tmp"

_default:
    @just --list

# Compile every package
build:
    spago build --strict

# Run the test suites
test:
    spago test -p sa-edition
    spago test -p sa-handlers

# The layers for impure work and the dependency direction (scripts/check/layers.js)
_layers:
    node scripts/check/layers.js

# Format all PureScript sources
fmt:
    purs-tidy format-in-place "packages/*/src/**/*.purs" "packages/*/test/**/*.purs" "apps/*/src/**/*.purs"

_fmt-check:
    purs-tidy check "packages/*/src/**/*.purs" "packages/*/test/**/*.purs" "apps/*/src/**/*.purs"

# fmt-check, build, layers, test
check: _fmt-check build _layers test

# The backend: reads the engine once a minute and serves GET /api/edition on 127.0.0.1:$PORT (default 8790)
# (ENGINE_URL, default https://scenario.hxrts.com)
backend: build
    node -e 'import("./output/Annual.Main/index.js").then(m => m.main())'

# The backend and the dev server together; the dev server proxies /api to the backend. The writer is off here unless
# WRITER=on: the deployed backend already writes each round once, and a second backend would prompt every round again.
dev: build
    #!/usr/bin/env bash
    set -euo pipefail
    WRITER="${WRITER:-off}" node -e 'import("./output/Annual.Main/index.js").then(m => m.main())' &
    trap 'kill $!' EXIT
    vite --config apps/newspaper/vite.config.mjs

# Production bundle in apps/newspaper/dist
bundle: build
    vite build --config apps/newspaper/vite.config.mjs

# Fetch the engine's OpenAPI contract into openapi/ (ENGINE_URL, default https://scenario.hxrts.com)
contract:
    mkdir -p openapi
    curl -fsS "${ENGINE_URL:-https://scenario.hxrts.com}/api/openapi/narrative.json" -o openapi/narrative.json
    curl -fsS "${ENGINE_URL:-https://scenario.hxrts.com}/api/openapi/transcription.json" -o openapi/transcription.json

_clean:
    rm -rf output .spago apps/newspaper/dist
