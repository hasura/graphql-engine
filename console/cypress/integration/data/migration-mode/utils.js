import { migrateModeUrl } from '../../../helpers/common';

export const toggleOnMigrationMode = () => {
  cy.request({
    method: 'GET',
    url: migrateModeUrl,
  }).then(response => {
    if (response.body.migration_mode === 'false') {
      // Go to migrations section
      cy.get('a')
        .contains('Migrations')
        .click();
      cy.wait(3000);
      // Toggle Migration mode
      cy.get('[class=react-toggle-track]').click();
      cy.wait(10000);
    }
  });
};

export const toggleOffMigrationMode = () => {
  cy.request({
    method: 'GET',
    url: migrateModeUrl,
  }).then(response => {
    if (response.body.migration_mode === 'true') {
      // Go to migrations section
      cy.get('a')
        .contains('Migrations')
        .click();
      cy.wait(3000);
      // Toggle Migration mode
      cy.get('[class=react-toggle-track]').click();
      cy.wait(10000);
    }
  });
};
