import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import tailwindcss from "@tailwindcss/vite";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
// The paper's own backend (`just backend`), which reads the engine; BACKEND_URL names another.
const backend = process.env.BACKEND_URL ?? `http://127.0.0.1:${process.env.PORT ?? 8790}`;

export default defineConfig({
  root: here,
  plugins: [react(), tailwindcss()],
  resolve: { alias: { "@output": path.resolve(here, "../../output") } },
  server: { port: 5180, proxy: { "/api": { target: backend, changeOrigin: true } } },
  build: { outDir: "dist", emptyOutDir: true },
});
