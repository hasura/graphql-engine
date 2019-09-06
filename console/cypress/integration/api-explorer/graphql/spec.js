/* eslint import/prefer-default-export: 0 */

import {
  getElementFromAlias,
  baseUrl,
  tableColumnTypeSelector,
} from '../../../helpers/dataHelpers';
import { validateCT } from '../../validators/validators';
import { makeDataAPIOptions } from '../../../helpers/dataHelpers';
import { toggleOnMigrationMode } from '../../data/migration-mode/utils';
// ***************** UTIL FUNCTIONS **************************

let adminSecret;
let dataApiUrl;

export const createTestTable = () => {
  cy.window().then(win => {
    adminSecret = win.__env.adminSecret;
    dataApiUrl = win.__env.dataApiUrl;
    const { consoleMode } = win.__env;
    if (consoleMode === 'cli') {
      toggleOnMigrationMode();
    }
  });

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
  // cy.get(getElementFromAlias('col-type-0')).click();
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_serial'))
    .first()
    .click();
  // cy.get(getElementFromAlias('col-type-0')).select('serial');
  cy.get(getElementFromAlias('column-1'))
    .clear()
    .type('name');
  tableColumnTypeSelector('col-type-1');
  cy.get(getElementFromAlias('data_test_column_type_value_text'))
    .first()
    .click();

  // cy.get(getElementFromAlias('col-type-1')).select('text');
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
  if (adminSecret) {
    cy.get(getElementFromAlias('header-key-2')).type('someKey');
    cy.get(getElementFromAlias('header-value-2')).type('someValue');
  } else {
    cy.get(getElementFromAlias('header-key-1')).type('someKey');
    cy.get(getElementFromAlias('header-value-1')).type('someValue');
  }

  cy.get('textarea')
    .first()
    .type('{enter}{uparrow}query{{}users{{}id}}', { force: true });
  cy.wait(1000);
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
  cy.wait(1000);
  cy.get('.execute-button').click();
  cy.wait(5000);
  cy.get('.cm-property').contains('id');
  cy.get('.cm-number').contains('2');
};

export const checkSub = () => {
  // Make a subscription
  cy.get('textarea')
    .first()
    .type(
      '{enter}{uparrow}#{leftarrow}{enter}{uparrow}subscription{{}users{{}name}}',
      { force: true }
    );
  cy.wait(1000);
  cy.get('.execute-button').click();
  cy.wait(5000);
  cy.get('.cm-property').contains('name');
  cy.get('.cm-string').contains('someName');
  // Update the user with id 1
  const reqBody = {
    type: 'update',
    args: {
      table: {
        name: 'users',
      },
      where: {
        id: '1',
      },
      $set: {
        name: 'someOtherName',
      },
    },
  };
  // Make the request
  const requestOptions = makeDataAPIOptions(dataApiUrl, adminSecret, reqBody);
  cy.request(requestOptions).then(res => {
    cy.log(JSON.stringify(res));
    cy.wait(3000);
    cy.get('.cm-string').contains('someOtherName');
    // cy.get('.cm-number').contains('1');
  });
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
