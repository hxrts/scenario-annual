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

# Format all PureScript sources
fmt:
    purs-tidy format-in-place "packages/*/src/**/*.purs" "packages/*/test/**/*.purs" "apps/*/src/**/*.purs"

_fmt-check:
    purs-tidy check "packages/*/src/**/*.purs" "packages/*/test/**/*.purs" "apps/*/src/**/*.purs"

# fmt-check, build, test
check: _fmt-check build test

# Dev server, proxying /api to the engine (ENGINE_URL, default https://scenario.hxrts.com)
dev: build
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
