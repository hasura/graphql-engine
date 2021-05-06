// ***********************************************
// This example commands.js shows you how to
// create various custom commands and overwrite
// existing commands.
//
// For more comprehensive examples of custom
// commands please read more here:
// https://on.cypress.io/custom-commands
// ***********************************************
//
//
// -- This is a parent command --
// Cypress.Commands.add("login", (email, password) => { ... })
//
//
// -- This is a child command --
// Cypress.Commands.add("drag", { prevSubject: 'element'}, (subject, options) => { ... })
//
//
// -- This is a dual command --
// Cypress.Commands.add("dismiss", { prevSubject: 'optional'}, (subject, options) => { ... })
//
//
// -- This is will overwrite an existing command --
// Cypress.Commands.overwrite("visit", (originalFn, url, options) => { ... })

Cypress.Commands.add('getBySel', (selector, ...args) =>
  cy.get(`[data-test=${selector}]`, ...args)
);

Cypress.Commands.add('getBySelLike', (selector, ...args) =>
  cy.get(`[data-test*=${selector}]`, ...args)
);

interface NotificationOptions {
  timeout?: number;
  type?: 'error' | 'success' | 'info';
}

Cypress.Commands.add(
  'checkNotification',
  (content: string, options?: NotificationOptions) => {
    const type = options?.type ?? 'success';
    const timeout = options?.timeout ?? 0;
    cy.get(`.notification-${type}`, timeout ? { timeout } : {})
      .contains(content)
      .should('be.visible')
      .click({ multiple: true, force: true });
  }
);

Cypress.Commands.add('setPrompt', (value: string, callback = () => {}) => {
  const sandbox = Cypress.sinon.createSandbox();
  cy.window()
    .then(win => {
      sandbox.stub(win, 'prompt').returns(value);
      callback();
      return null;
    })
    .then(() => {
      sandbox.restore();
    });
});
