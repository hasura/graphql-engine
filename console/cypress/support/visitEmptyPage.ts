/**
 * Visit the initial empty page.
 *
 * @see https://glebbahmutov.com/blog/visit-blank-page-between-tests/
 */
Cypress.Commands.add('visitEmptyPage', { prevSubject: false }, () => {
  cy.window().then(win => {
    win.location.href = 'about:blank';
  });
});
