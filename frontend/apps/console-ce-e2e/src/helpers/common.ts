import { getTestMode } from './core/testMode';

export const testMode = getTestMode();
export const migrateUrl = Cypress.env('MIGRATE_URL');
export const migrateModeUrl = `${migrateUrl}/settings`;

// ex. http://localhost:4200/
const cypressBaseUrl = Cypress.config('baseUrl');

// ex. http://localhost:4200
export const baseUrl = cypressBaseUrl.endsWith('/')
  ? cypressBaseUrl.slice(0, -1)
  : cypressBaseUrl;

// sets value of window.prompt and reloads page
export const setPromptValue = (value: string | null) => {
  cy.log(`Set window.prompt to "${value}"`).then(() => {
    cy.removeAllListeners('window:before:load');
    cy.on('window:before:load', win => {
      cy.stub(win, 'prompt').returns(value);
    });
  });

  cy.reload();
};

// This is works as setPromptValue with no unnecessary waiting
export const setPromptWithCb = (value: string | null, cb: () => void) => {
  cy.window().then(win => {
    cy.stub(win, 'prompt').returns(value);
    cb();
  });
};
