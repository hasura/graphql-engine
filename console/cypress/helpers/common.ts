export const testMode = Cypress.env('TEST_MODE');
export const baseUrl = Cypress.config('baseUrl');
export const migrateUrl = Cypress.env('MIGRATE_URL');
export const migrateModeUrl = `${migrateUrl}/settings`;

/**
 * sets value of window.prompt and reloads page
 * @param value
 * @deprecated Use `cy.setPrompt(value, callBackFn)` instead
 */
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

/**
 * sets value of window.prompt and reloads page
 * @param value
 * @deprecated Use `cy.setPrompt(value, callBackFn)` instead
 */
export const setPromptWithCb = (value: string | null, cb: () => void) => {
  const sandbox = Cypress.sinon.createSandbox();
  cy.window()
    .then(win => {
      sandbox.stub(win, 'prompt').returns(value);
      cb();
      return null;
    })
    .then(() => {
      sandbox.restore();
    });
};
