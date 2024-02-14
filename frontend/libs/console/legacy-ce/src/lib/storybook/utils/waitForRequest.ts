import { getWorker } from 'msw-storybook-addon';
import { MockedRequest, matchRequestUrl } from 'msw';

// https://mswjs.io/docs/extensions/life-cycle-events#asserting-request-payload
export function waitForRequest(method: string, url: string, suffix: string) {
  const worker = getWorker();
  let requestId = '';

  return new Promise<MockedRequest>((resolve, reject) => {
    worker.events.on('request:start', async req => {
      const matchesMethod = req.method.toLowerCase() === method.toLowerCase();
      const matchesUrl = matchRequestUrl(req.url, url).matches;
      try {
        // Clone to avoid "locked body stream" error
        // https://stackoverflow.com/a/54115314
        const body = await req.clone().json();
        const matchesSuffix = body.type.endsWith(suffix);

        if (matchesMethod && matchesUrl && matchesSuffix) {
          requestId = req.id;
        }
      } catch (error) {
        console.error(error);
      }
    });

    worker.events.on('request:match', req => {
      if (req.id === requestId) {
        resolve(req);
      }
    });

    worker.events.on('request:unhandled', req => {
      if (req.id === requestId) {
        reject(
          new Error(`The ${req.method} ${req.url.href} request was unhandled.`)
        );
      }
    });
  });
}
