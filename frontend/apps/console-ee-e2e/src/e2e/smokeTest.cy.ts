// The main reason behind this e2e is to make sure we don’t have circular dependency that block loading the console at all
describe('console-ee is loading', () => {
  beforeEach(() => cy.visit('/'));

  it('should display welcome message', () => {
    // Graphiql tab loading properly
    cy.get('[data-test="graphiql-explorer-link"]').contains('GraphiQL');

    // Go to data tab
    cy.get('[data-test="data-tab-link"]').click();

    // Data tab is loaded
    cy.get('[data-test="sql-link"]').contains('SQL');

    // Go to action tab
    cy.get('[data-test="actions-tab-link"]').click();

    // Action tab is loaded
    cy.get('[data-test="data-create-actions"]').contains('Create');

    // Go to remote schema tab
    cy.get('[data-test="remote schemas-tab-link"]').click();

    // Remote schema tab is loaded
    cy.get('[data-test="data-create-remote-schemas"]').contains('Add');

    // Go to the event tab
    cy.get('[data-test="events-tab-link"]').click();

    // Event tab is loaded
    cy.get('[data-test="data-create-trigger"]').contains('Create');

    // Go to the settings page
    cy.get('[href="/settings"]').click();

    // Setting page is loaded
    cy.get('[data-test="data-export-metadata"]').contains('Export metadata');
  });
});
