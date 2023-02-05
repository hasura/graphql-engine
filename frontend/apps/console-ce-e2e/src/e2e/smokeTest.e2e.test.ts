// The main reason behind this e2e is to make sure we don’t have circular dependency that block loading the console at all
describe('Console CE', () => {
  beforeEach(() => cy.visit('/'));

  // The main goal of this E2E test is to make sure we don’t have circular dependencies that block
  // loading the console at all
  it('Should work', () => {
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

    // Temporary disable the settings check because
    // `<a href="/settings">...</a>`
    //  is being covered by another element:
    //  `<div class="font-semibold text-base w-full" style="color: rgb(54, 156, 199);">Telemetry</div>`
    // Go to the settings page
    // cy.get('[href="/settings"]').click();
    // Setting page is loaded
    // cy.get('[data-test="data-export-metadata"]').contains('Export metadata');
  });
});
