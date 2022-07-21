Cypress.Commands.add('expectSuccessNotification', () => {
  const el = cy.get('.notification-success');
  el.should('be.visible');
});

Cypress.Commands.add('expectSuccessNotificationWithTitle', (title: string) => {
  const el = cy.get('.notification-success');
  el.should('be.visible');
  el.should('contain', title);
});

Cypress.Commands.add(
  'expectSuccessNotificationWithMessage',
  (message: string) => {
    const el = cy.get('.notification-success');
    el.should('be.visible');
    el.should('contain', message);
  }
);

Cypress.Commands.add('expectErrorNotification', () => {
  const el = cy.get('.notification-error');
  el.should('be.visible');
});

Cypress.Commands.add('expectErrorNotificationWithTitle', (title: string) => {
  const el = cy.get('.notification-error');
  el.should('be.visible');
  el.should('contain', title);
});

Cypress.Commands.add(
  'expectErrorNotificationWithMessage',
  (message: string) => {
    const el = cy.get('.notification-error');
    el.should('be.visible');
    el.should('contain', message);
  }
);
