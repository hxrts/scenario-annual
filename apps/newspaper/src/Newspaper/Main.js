const now = new Date();
export const today = { year: now.getFullYear(), month: now.getMonth() + 1, day: now.getDate() };
export const longDate = (d) =>
  new Date(d.year, d.month - 1, d.day).toLocaleDateString("en-GB", { weekday: "long", day: "numeric", month: "long", year: "numeric" });
