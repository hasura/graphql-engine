// Classic workers cannot be constructed from a different origin, even when the
// CDN sends CORS headers. Bootstrap from a same-origin blob instead, and keep
// the original URL so webpack can load the worker's split chunks from the CDN.
// This constructor is injected ONLY into GraphiQL's webpack worker setup.
class MonacoCdnWorker extends globalThis.Worker {
  constructor(scriptURL, options) {
    const url = new URL(scriptURL, globalThis.location.href);
    const bootstrapURL =
      url.origin === globalThis.location.origin
        ? undefined
        : URL.createObjectURL(
            new Blob(
              [
                `self.__HASURA_MONACO_WORKER_URL__ = ${JSON.stringify(url.href)};\n` +
                  `importScripts(${JSON.stringify(url.href)});`,
              ],
              { type: 'text/javascript' },
            ),
          );

    try {
      super(bootstrapURL ?? url, options);
    } catch (error) {
      if (bootstrapURL) URL.revokeObjectURL(bootstrapURL);
      throw error;
    }

    let released = false;
    this.releaseBootstrap = () => {
      if (!bootstrapURL || released) return;
      released = true;
      URL.revokeObjectURL(bootstrapURL);
      this.removeEventListener('message', this.releaseBootstrap);
      this.removeEventListener('error', this.releaseBootstrap);
    };

    // Wait until the worker has loaded, not just until its constructor returns.
    // Monaco's first reply acknowledges initialization; errors and early
    // termination must release the blob too.
    if (bootstrapURL) {
      this.addEventListener('message', this.releaseBootstrap);
      this.addEventListener('error', this.releaseBootstrap);
    }
  }

  terminate() {
    this.releaseBootstrap();
    return super.terminate();
  }
}

module.exports = { MonacoCdnWorker };
