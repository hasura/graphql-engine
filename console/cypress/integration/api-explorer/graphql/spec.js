/* eslint import/prefer-default-export: 0 */

import { getElementFromAlias, baseUrl } from '../../../helpers/dataHelpers';
import { validateCT } from '../../validators/validators';

export const createTestTable = () => {
  //    Click on the create table button
  cy.visit('/data/schema');
  cy.wait(15000);
  cy.get(getElementFromAlias('data-create-table')).click();
  // Enter the table name
  cy.get(getElementFromAlias('tableName')).type('users');
  //   Set first column
  cy.get(getElementFromAlias('column-0'))
    .clear()
    .type('id');
  cy.get(getElementFromAlias('col-type-0')).select('serial');
  cy.get(getElementFromAlias('column-1'))
    .clear()
    .type('name');
  cy.get(getElementFromAlias('col-type-1')).select('text');
  //   Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  //  Click on create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(10000);
  //  Check if the table got created and navigatied to modify table
  cy.url().should('eq', `${baseUrl}/data/schema/public/tables/users/modify`);
  //   Validate
  validateCT('users', 'success');
};

export const insertValue = () => {
  cy.get(getElementFromAlias('table-insert-rows')).click();
  // Insert a row
  cy.get(getElementFromAlias('typed-input-1')).type('someName');
  cy.get(getElementFromAlias('insert-save-button')).click();
};

export const openAPIExplorer = () => {
  //eslint-disable-line
  // Open API Explorer
  cy.get(getElementFromAlias('api-explorer')).click();
  cy.wait(3000);
};

export const checkExecuteQueryButton = () => {
  cy.get('.execute-button').click();
  cy.get('.cm-def').contains('errors');
};

export const checkQuery = () => {
  cy.get('textarea')
    .first()
    .type('{enter}{uparrow}query{{}users{{}id}}', { force: true });
  cy.get('.execute-button').click();
  cy.get('.cm-property').contains('id');
  cy.get('.cm-number').contains('1');
};

export const checkMutation = () => {
  cy.get('textarea')
    .first()
    .type(
      '{enter}{uparrow}#{leftarrow}{enter}{uparrow}mutation insert_user{{}insert_users(objects:[{{}name:"someName"}]){{}returning{{}id}}}',
      { force: true }
    );
  cy.get('.execute-button').click();
  cy.get('.cm-property').contains('id');
  cy.get('.cm-number').contains('2');
};

export const delTestTable = () => {
  cy.get('a')
    .contains('Data')
    .click();
  //   Go to the modify section of the table
  cy.get(getElementFromAlias('users')).click();
  cy.get(getElementFromAlias('table-modify')).click();
  //   Click on delete
  cy.get(getElementFromAlias('delete-table')).click();
  //   Confirm
  cy.on('window:confirm', str => {
    expect(str === 'Are you sure?').to.be.true;
    return true;
  });
  cy.wait(7000);
  //   Match the URL
  cy.url().should('eq', `${baseUrl}/data/schema/public`);
  //   Validate
  validateCT('users', 'failure');
};
