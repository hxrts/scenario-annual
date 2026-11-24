export const envImpl = (name) => () => process.env[name] ?? null;
export const writeLine = (line) => () => console.log(`${new Date().toISOString()} ${line}`);
export const everyImpl = (ms, action) => () => {
  setInterval(action, ms);
};
