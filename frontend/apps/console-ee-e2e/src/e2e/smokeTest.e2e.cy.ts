// The main reason behind this e2e is to make sure we don’t have circular dependency that block loading the console at all
describe('Console EE', () => {
  beforeEach(() => cy.visit('/'));

  // The main goal of this E2E test is to make sure we don’t have circular dependencies that block
  // loading the console at all
  it('Should work', () => {
    // Graphiql tab loading properly
    cy.get('[data-test="graphiql-explorer-link"]').contains('GraphiQL');

    // Go to data tab
    cy.findByTestId('Nav bar').findByText('Data').click();

    // Data tab is loaded
    cy.findByTestId('Data Manager').contains('Data Manager');

    // Go to action tab
    cy.findByTestId('Nav bar').findByText('Actions').click();

    // Action tab is loaded
    cy.findByTestId('data-create-actions').contains('Create');

    // Go to remote schema tab
    cy.findByTestId('Nav bar').findByText('Remote Schemas').click();

    // Remote schema tab is loaded
    cy.findByTestId('data-create-remote-schemas').contains('Add');

    // Go to the event tab
    cy.findByTestId('Nav bar').findByText('Events').click();

    // Event tab is loaded
    cy.findByTestId('data-create-trigger').contains('Create');

    // Go to the settings page
    cy.get('[href$="/settings"]').click();

    // Setting page is loaded
    cy.findByTestId('data-export-metadata').contains('Export metadata');
  });
});
