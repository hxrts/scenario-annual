import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import tailwindcss from "@tailwindcss/vite";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
// The live engine by default; set ENGINE_URL=http://localhost:8787 for a local `just serve` in scenario-engine.
const engine = process.env.ENGINE_URL ?? "https://scenario.hxrts.com";

export default defineConfig({
  root: here,
  plugins: [react(), tailwindcss()],
  resolve: { alias: { "@output": path.resolve(here, "../../output") } },
  server: { port: 5180, proxy: { "/api": { target: engine, changeOrigin: true } } },
  build: { outDir: "dist", emptyOutDir: true },
});
