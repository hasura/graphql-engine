import { setPromptWithCb } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';

export const navigateAndOpenConnectDatabasesForm = () => {
  cy.location('pathname').then(currentPage => {
    const alreadyOnThePage = currentPage.startsWith('/data/manage');
    if (!alreadyOnThePage) {
      cy.log('**--- Navigate to Connect Databases Form**');

      cy.log('**--- visit index route and set metadata**');
      cy.visit('/data/default/schema/public').then(setMetaData);

      cy.log('**--- Click on the manage database menu**');
      cy.findByRole('button', { name: 'Manage' }).click();
      cy.location('pathname').should('eq', '/data/manage');
    }
  });

  cy.log('**--- Click on the Connect Database button**');
  cy.findByRole('button', { name: 'Connect Database' }).click();
  cy.location('pathname').should('eq', '/data/manage/connect');

  cy.get('form').within(() => {
    cy.log('**--- Click on Connection Settings section**');
    cy.contains('Connection Settings').click();
  });
  cy.get('form').within(() => {
    cy.log('**--- Click on GraphQL Field Customization section**');
    cy.contains('GraphQL Field Customization').click();
  });
};

export const navigateToManageDatabases = () => {
  cy.log('**--- Visit index route and set metadata**');
  cy.visit('/data/default/schema/public').then(setMetaData);

  cy.log('**--- Visit the manage database route**');
  cy.getBySel('sidebar-manage-database').click();
  cy.location('pathname').should('eq', '/data/manage');
};

export const submitConnectDBForm = () => {
  cy.log('**--- Click on the Connect Database button**');
  cy.getBySel('connect-database-btn').click();
};

export const removeDBFromList = (dbName: string) => {
  setPromptWithCb(dbName, () => {
    cy.log('**--- Click on the Remove Database button**');
    cy.getBySel(dbName).find('button').contains('Remove').click();
  });
};
