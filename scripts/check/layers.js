#!/usr/bin/env node
// The layers for impure work, as in scenario-engine (its docs/08 §1):
//   edition   pure: no Effect, Aff or FFI
//   effects   rows and programs: no Effect, Aff, Node, handler or adapter import
//   handlers  interpreters over devices: no adapter import
//   adapters  raw devices: no paper module, and no paper word
//   only the composition root (apps/annual/src/Annual/Main.purs) and tests import adapters
// and each workspace package depends only on the packages its row below names.
const fs = require("fs");
const path = require("path");

const root = path.resolve(__dirname, "../..");
const allowed = {
  "sa-edition": [],
  "sa-effects": ["sa-edition"],
  "sa-handlers": ["sa-edition", "sa-effects"],
  "sa-adapters": [],
  "sa-annual": ["sa-adapters", "sa-effects", "sa-handlers"],
  "sa-newspaper": ["sa-edition"],
};
const compositionRoots = new Set(["apps/annual/src/Annual/Main.purs"]);

const walk = (dir) =>
  fs.existsSync(dir)
    ? fs.readdirSync(dir, { withFileTypes: true }).flatMap((e) => {
        const p = path.join(dir, e.name);
        return e.isDirectory() ? walk(p) : [path.relative(root, p)];
      })
    : [];
const files = ["packages", "apps"].flatMap((d) => walk(path.join(root, d))).filter((f) => !f.includes("/dist/"));
const imports = (f) => fs.readFileSync(path.join(root, f), "utf8").split("\n").filter((l) => /^import\s/.test(l));
const isTest = (f) => f.includes("/test/");
const under = (dir) => (f) => f.startsWith(dir) && !isTest(f);

const rules = [
  { name: "edition is pure: no Effect, Aff or Node import", files: under("packages/edition/src/"), bad: (l) => /^import\s+(Effect|Node)(\.|\s|$)/.test(l) },
  { name: "effects declare rows and programs: no Effect, Aff, Node, handler or adapter import", files: under("packages/effects/src/"), bad: (l) => /^import\s+(Effect|Node|Handlers|Adapters)(\.|\s|$)/.test(l) },
  { name: "adapters are raw devices: no Edition, Effects or Handlers import", files: under("packages/adapters/src/"), bad: (l) => /^import\s+(Edition|Effects|Handlers)(\.|\s|$)/.test(l) },
  { name: "only the composition root and tests import an adapter", files: (f) => f.endsWith(".purs") && !compositionRoots.has(f) && !isTest(f), bad: (l) => /^import\s+Adapters\./.test(l) },
];

let failures = [];
for (const rule of rules)
  for (const f of files.filter((f) => f.endsWith(".purs") && rule.files(f)))
    for (const l of imports(f)) if (rule.bad(l)) failures.push(`${f}: ${l}\n  ${rule.name}`);

// FFI belongs to adapters and the browser app only.
for (const f of files.filter((f) => f.endsWith(".js") && f.includes("/src/")))
  if (!f.startsWith("packages/adapters/src/") && !f.startsWith("apps/newspaper/src/")) failures.push(`${f}\n  FFI lives only in adapters (and the browser app)`);

// An adapter knows nothing of the paper.
for (const f of files.filter(under("packages/adapters/src/")))
  if (/\b(issues?|editions?|reports?|rounds?|engine|newspaper|desk)\b/i.test(fs.readFileSync(path.join(root, f), "utf8")))
    failures.push(`${f}\n  an adapter knows nothing of the paper: no paper word in a device`);

// Dependency direction.
for (const f of files.filter((f) => f.endsWith("spago.yaml"))) {
  const text = fs.readFileSync(path.join(root, f), "utf8");
  const name = (text.match(/name:\s*(\S+)/) || [])[1];
  if (!name || !(name in allowed)) continue;
  const src = text.split(/\n\s*test:/)[0];
  for (const dep of src.match(/\bsa-[a-z]+\b/g).filter((d) => d !== name))
    if (!allowed[name].includes(dep)) failures.push(`${f}: ${name} depends on ${dep}\n  allowed: ${allowed[name].join(", ") || "no workspace package"}`);
}

if (failures.length) {
  console.error(failures.join("\n"));
  process.exit(1);
}
console.log("layers ok");
