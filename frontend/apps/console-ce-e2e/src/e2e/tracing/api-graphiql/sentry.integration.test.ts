describe('Sentry', () => {
  // Verifies that when a Sentry DSN is configured, the Console actually starts
  // Sentry tracing (it fires a request to the Sentry ingest endpoint).
  it('When HASURA_CONSOLE_SENTRY_DSN is set, then Sentry should start tracing', () => {
    // Stub the Sentry ingest endpoint BEFORE visiting the Console. This both
    // (a) prevents any request leaking to the external Sentry server, and
    // (b) removes the race that existed when the intercept was registered inside
    //     the test, AFTER the Console had already loaded in a `before()` hook.
    // We do NOT stub/suppress the Console's own backend requests: the Console
    // boots against the real server (its live bootstrap requests), which is what
    // the previous stale per-request fixtures were trying — and failing — to fake.
    cy.intercept('https://**.ingest.sentry.io/**', {
      statusCode: 200,
      body: {},
    }).as('sentryRequest');

    cy.visit('/', {
      onBeforeLoad: (window) => {
        Cypress.log({
          message: '**--- Fake the `consoleSentryDsn` env variable**',
        });

        function recursivelyTryToSetConsoleSentryDsn() {
          if (!window.__env) {
            // The page has not been loaded yet and window.__env is not available
            setTimeout(recursivelyTryToSetConsoleSentryDsn, 10);
            return;
          }

          const consoleSentryDsnAlreadyExists =
            !!window.__env.consoleSentryDsn &&
            window.__env.consoleSentryDsn !== 'undefined';

          if (consoleSentryDsnAlreadyExists) {
            return;
          }

          // Without a consoleSentryDsn env variable, Sentry tracing is not
          // started. This is a fake DSN (a real one modified to avoid exposing
          // the original); the ingest endpoint is stubbed above so nothing leaves
          // the test.
          window.__env.consoleSentryDsn =
            'https://99942022c9cc4306aa4084ef90f307ff@o417608.ingest.sentry.io/6684052';
        }

        setTimeout(recursivelyTryToSetConsoleSentryDsn, 10);
      },
    });

    // The only fact that a request has been performed to ingest.sentry.io is
    // enough to safely say that Sentry is up and running.
    cy.log('**--- Check that Sentry started tracing (called the ingest API)**');
    cy.wait('@sentryRequest');
  });
});
