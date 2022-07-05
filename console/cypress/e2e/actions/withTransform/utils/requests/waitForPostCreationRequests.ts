/**
 * Wait for a bunch of requests to be settled before proceeding with the test.
 *
 * Alternatively, https://github.com/bahmutov/cypress-network-idle could be used
 *
 * This is a workaround for "element is 'detached' from the DOM" Cypress' error (see the issue
 * linked below). Since the UI gets re-rendered because of the requests, this utility ensures that
 * all the requests parallelly made by the UI are settled before proceeding with the test. Hance, it
 * ensure the UI won't re-render during the next interaction.
 *
 * What are the requests that must be awaited? By looking at the Cypress Test Runner, they are the
 * following, made parallelly or in a rapid series.
 * 1. export_metadata
 * 2. export_metadata
 * 3. export_metadata
 * 4. test_webhook_transform
 * 5. test_webhook_transform
 * 6. test_webhook_transform
 * 7. test_webhook_transform
 * At the moment of writing, I'm not sure the number of requests are fixed or not. If they are fixed,
 * using the cy.intercept `times` options would result in a more expressive and less convoluted code.
 *
 * To give you an overall idea, this is a timeline of the requests
 *
 *         all requests start                             all requests end
 *         |                 |                            |               |
 * |--🚦🔴--1--2--3--4--5--6--7----------------------------1--2--3--4--5--6-7--🚦🟢--|
 *
 *
 * ATTENTION: Despite the defensive approach and the flakiness-removal purpose, this function could
 * introduced even more flakiness because of its empiric approach. In case of failures, it must be
 * carefully evaluated when/if keeping it or thinking about a better approach.
 * In generale, this solution does not scale, at should not be spread among the tests.
 *
 * @see https://github.com/cypress-io/cypress/issues/7306
 * @see https://glebbahmutov.com/blog/detached/
 * @see https://github.com/bahmutov/cypress-network-idle
 */
export function waitForPostCreationRequests() {
  let waitCompleted = false;

  cy.log('*--- All requests must be settled*');

  const pendingRequests = new Map();
  cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
    if (waitCompleted) return;

    Cypress.log({ message: '*--- Request pending*' });

    pendingRequests.set(req, true);

    req.continue(() => {
      Cypress.log({ message: '*--- Request settled*' });

      pendingRequests.delete(req);
    });
  });

  Cypress.log({ message: '*--- Waiting for the first request to start*' });

  // Check if at least one request has been caught. This check must protect from the following case
  //
  //            check          requests start           test failure, the requests got the UI re-rendered
  //            |              |                        |
  // |--🚦🔴----⚠️---🚦🟢-------1-2-3-4-5-6-7-1----------💥
  //
  // where checking that "there are no pending requests" falls in the false positive case where
  // there are no pending requests because no one started at all.
  //
  // The check runs every millisecond to be 100% sure that no request can escape (ex. because of a
  // super fast server). A false-negative case represented here
  //
  //         requests start requests end   check              check               test failure, no first request caught
  //         |            | |           |  |                  |                   |
  // |--🚦🔴--1-2-3-4-5-6-7-1-2-3-4-5-6-7--⚠️------------------⚠️------------------💥
  cy.waitUntil(() => pendingRequests.size > 0, {
    timeout: 5000, // 5 seconds is the default Cypress wait for a request to start
    interval: 1,
    errorMsg: 'No first request caught',
  });

  Cypress.log({ message: '*--- Waiting for all the requests to start*' });

  // Let pass some time to collect all the requests. Otherwise, it could detect that the first
  // request complete and go on with the test, even if another one will be performed in a while.
  //
  // This fixed wait protects from the following timeline
  //
  //           1st request start     first request end       other requests start   test failure, the requests got the UI re-rendered
  //           |                     |                       |                      |
  // |--🚦🔴---1---------------------1----🚦🟢----------------2-3-4-5-6-7-1----------💥
  //
  // Obviously, it is an empiric waiting, that also slows down the test.
  cy.wait(500);

  Cypress.log({ message: '*--- Waiting for all the requests to be settled*' });

  cy.waitUntil(() => pendingRequests.size === 0, {
    timeout: 30000, // 30 seconds is the default Cypress wait for the request to complete
    errorMsg: 'Some requests are not settled yet',
  }).then(() => {
    waitCompleted = true;
  });
}
