import type { SetupWorker } from 'msw/browser';

// TODO(msw-v2-migration): msw-storybook-addon v3 dropped the standalone
// `getWorker()` export that this helper used to fetch the active worker
// instance (see https://github.com/mswjs/msw-storybook-addon, "Custom worker
// setup" / story context section). The addon now only exposes the worker via
// the story's `msw` context property (e.g. `beforeEach({ msw }) { ... }` or
// the `play` function's context), so callers of this helper must now obtain
// the `SetupWorker` themselves (from story context) and pass it in explicitly.
// This function has no active callers in the codebase today (the only call
// sites are commented out in LogicalModelWidget.stories.tsx), so the safest
// fix was to change the signature rather than guess at a global accessor that
// no longer exists. If/when this helper is reactivated, wire it up via the
// story's `msw` context per the addon's v3 docs.
// https://mswjs.io/docs/api/life-cycle-events
export function waitForRequest(
  worker: SetupWorker,
  method: string,
  url: string,
  suffix: string,
) {
  let requestId = '';

  return new Promise<Request>((resolve, reject) => {
    worker.events.on('request:start', async ({ request, requestId: id }) => {
      const matchesMethod =
        request.method.toLowerCase() === method.toLowerCase();
      const matchesUrl = request.url === url || request.url.startsWith(url);
      try {
        // Clone to avoid "locked body stream" error
        // https://stackoverflow.com/a/54115314
        const body = await request.clone().json();
        const matchesSuffix = body.type.endsWith(suffix);

        if (matchesMethod && matchesUrl && matchesSuffix) {
          requestId = id;
        }
      } catch (error) {
        console.error(error);
      }
    });

    worker.events.on('request:match', ({ request, requestId: id }) => {
      if (id === requestId) {
        resolve(request);
      }
    });

    worker.events.on('request:unhandled', ({ request, requestId: id }) => {
      if (id === requestId) {
        reject(
          new Error(
            `The ${request.method} ${request.url} request was unhandled.`,
          ),
        );
      }
    });
  });
}
