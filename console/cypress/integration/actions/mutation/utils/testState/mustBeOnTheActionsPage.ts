/**
 * Ensure the test is on the Actions page.
 */
export function mustBeOnTheActionsPage() {
  cy.dataSession({
    name: 'mustBeOnTheActionsPage',

    validate: () => {
      return cy.location('pathname').then(currentPage => {
        return currentPage.startsWith('/actions/manage');
      });
    },

    preSetup: () =>
      Cypress.log({ message: '**--- The Actions page must be visited**' }),

    setup: () => {
      cy.visit('/actions/manage/actions').location('pathname');
    },
  });
}
