export const testMode = Cypress.env('TEST_MODE');
export const baseUrl = Cypress.config('baseUrl');
export const migrateUrl = Cypress.env('MIGRATE_URL');
export const migrateModeUrl = `${migrateUrl}/settings`;

// sets value of window.prompt and reloads page
export const setPromptValue = (value: string | null) => {
  cy.log(`Set window.prompt to "${value}"`).then(() => {
    cy.removeAllListeners('window:before:load');
    cy.on('window:before:load', win => {
      cy.stub(win, 'prompt').returns(value);
    });
  });

  cy.reload();

  /* eslint-disable-next-line cypress/no-unnecessary-waiting */
  cy.wait(7000);
};

// This is works as setPromptValue with no unnecessary waiting
export const setPromptWithCb = (value: string | null, cb: () => void) => {
  cy.window().then(win => {
    cy.stub(win, 'prompt').returns(value);
    cb();
  });
};
