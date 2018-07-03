import { migrateModeUrl } from '../../../helpers/common';

export const toggleOnMigrationMode = () => {
  cy.request({
    method: 'GET',
    url: migrateModeUrl,
  }).then(response => {
    if (response.body.migration_mode === 'false') {
      cy.visit('/data/migrations');
      cy.wait(5000);
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
      // eslint-disable-line
      cy.visit('/data/migrations');
      cy.wait(5000);
      cy.get('[class=react-toggle-track]').click();
      cy.wait(10000);
    }
  });
};
