import { existsSync, mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { dirname } from "node:path";

export const readImpl = (path) => () => (existsSync(path) ? readFileSync(path, "utf8") : null);

export const writeImpl = (path) => (text) => () => {
  mkdirSync(dirname(path), { recursive: true });
  const temporary = `${path}.${process.pid}.tmp`;
  writeFileSync(temporary, text);
  renameSync(temporary, path);
};
