Cypress.Commands.add('expectSuccessNotification', () => {
  cy.get('[data-testid=notification][data-notificationtype=success]').should(
    'be.visible'
  );
});

Cypress.Commands.add('expectSuccessNotificationWithTitle', (title: string) => {
  cy.get('[data-testid=notification][data-notificationtype=success]')
    .should('be.visible')
    .should('contain', title);
});

Cypress.Commands.add(
  'expectSuccessNotificationWithMessage',
  (message: string) => {
    cy.get('[data-testid=notification][data-notificationtype=success]')
      .should('be.visible')
      .should('contain', message);
  }
);

Cypress.Commands.add('expectErrorNotification', () => {
  cy.get('[data-testid=notification][data-notificationtype=error]').should(
    'be.visible'
  );
});

Cypress.Commands.add('expectErrorNotificationWithTitle', (title: string) => {
  cy.get('[data-testid=notification][data-notificationtype=error]')
    .should('be.visible')
    .should('contain', title);
});

Cypress.Commands.add(
  'expectErrorNotificationWithMessage',
  (message: string) => {
    cy.get('[data-testid=notification][data-notificationtype=error]')
      .should('be.visible')
      .should('contain', message);
  }
);
