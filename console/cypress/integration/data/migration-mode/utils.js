export const migrateModeUrl = 'http://localhost:9693/apis/migrate/settings';

export const toggleOnMigrationMode = () => {
  cy.request({
    method: 'GET',
    url: migrateModeUrl,
  }).then(response => {
    if (response.body.migration_mode === 'false') {
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
      cy.get('[class=react-toggle-track]').click();
      cy.wait(10000);
    }
  });
};
