import http from "node:http";

export const getImpl = (timeout, url) => (onError, onSuccess) => {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(new Error(`no answer in ${timeout}ms`)), timeout);
  fetch(url, { signal: controller.signal })
    .then(async (res) => onSuccess({ status: res.status, body: await res.text() }))
    .catch((e) => onError(e instanceof Error ? e : new Error(String(e))))
    .finally(() => clearTimeout(timer));
  return (cancelError, onCancelerError, onCancelerSuccess) => {
    controller.abort(cancelError);
    onCancelerSuccess();
  };
};

export const listenImpl = (port, host, handler) => () => {
  http
    .createServer((req, res) => {
      const reply = handler({
        method: req.method ?? "GET",
        path: new URL(req.url ?? "/", "http://localhost").pathname,
        header: (name) => {
          const v = req.headers[name.toLowerCase()];
          return Array.isArray(v) ? v.join(", ") : (v ?? null);
        },
      })();
      const headers = { "content-type": "application/json; charset=utf-8" };
      for (const h of reply.headers) headers[h.name] = h.value;
      res.writeHead(reply.status, headers);
      res.end(req.method === "HEAD" || reply.status === 304 ? undefined : reply.body);
    })
    .listen(port, host);
};
