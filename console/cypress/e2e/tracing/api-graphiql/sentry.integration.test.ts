import { blockServerRequests } from './utils/blockServerRequests';
import {
  stubInitialServerRequests,
  waitForInitialServerRequests,
} from './fixtures/initialRequests/stubInitialServerRequests';

describe('Sentry', () => {
  before(() => {
    cy.log('**--- Start controlling the server**');
    blockServerRequests();
    stubInitialServerRequests();

    cy.log('**--- Load the Console**');
    cy.visit('/', {
      onBeforeLoad: window => {
        Cypress.log({
          message: '**--- Fake the `consoleSentryDsn` env variable**',
        });

        function recursivelyTryToSetConsoleSentryDsn() {
          if (!window.__env) {
            // The page has not been loaded yet and the window.__env is not available yet
            setTimeout(recursivelyTryToSetConsoleSentryDsn, 10);
            return;
          }

          const consoleSentryDsnAlreadyExists =
            !!window.__env.consoleSentryDsn &&
            window.__env.consoleSentryDsn !== 'undefined';

          if (consoleSentryDsnAlreadyExists) {
            Cypress.log({
              message: '**--- A `consoleSentryDsn` env var already exists**',
            });
            return;
          }

          // Without a consoleSentryDsn env variable, we do not start Sentry tracing.
          // Here we set a fake consoleSentryDsn (usually it's not defined in the dev local machine)
          // before the Console starts
          window.__env.consoleSentryDsn =
            // This Sentry DSN does not exist, it's a real one modified to avoid exposing the original
            // one publicly
            'https://99942022c9cc4306aa4084ef90f307ff@o417608.ingest.sentry.io/6684052';

          Cypress.log({
            message: '**--- A Fake `consoleSentryDsn` env var has been set**',
          });
        }

        setTimeout(recursivelyTryToSetConsoleSentryDsn, 10);
      },
    });

    waitForInitialServerRequests();
  });

  // Check that we do not mess up with the Sentry integration/configuration
  it('When HASURA_CONSOLE_SENTRY_DSN is set, then Sentry should start tracing', () => {
    // Sentry sends requests to URLs like this
    // https://o417608.ingest.sentry.io/api/6684052/envelope/?sentry_key=99942022c9cc4306aa4084ef90f307ff&sentry_version=7&sentry_client=sentry.javascript.react%2F7.11.1
    cy.intercept('https://**.ingest.sentry.io/**').as('sentryRequest');

    // The only fact that a request has been performed to ingest.sentry.io is enough to safely
    // say that Sentry is up and running
    cy.log(
      '**--- Check that Sentry called the Sentry server to start tracing errors**'
    );
    cy.wait('@sentryRequest');
  });
});
