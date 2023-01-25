import {
  tableColumnTypeSelector,
  baseUrl,
  getTableName,
  getElementFromAlias,
  getColName,
  queryTypes,
} from '../../../helpers/dataHelpers';

import { setMetaData } from '../../validators/validators';

import { testPermissions, permRemove, createView, trackView } from './utils';
import { setPromptValue } from '../../../helpers/common';

const testName = 'perm';

export const passPTCreateTable = () => {
  // Click on create table
  cy.get(getElementFromAlias('data-create-table')).click();
  // Match the URL
  cy.url().should('eq', `${baseUrl}/data/default/schema/public/table/add`);
  // Type table name
  cy.get(getElementFromAlias('tableName')).type(getTableName(0, testName));
  // Set first column
  cy.get(getElementFromAlias('column-0')).type(getColName(0));
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_serial'))
    .first()
    .click();
  // Set second column
  cy.get(getElementFromAlias('column-1')).type(getColName(1));
  tableColumnTypeSelector('col-type-1');
  cy.get(getElementFromAlias('data_test_column_type_value_integer'))
    .first()
    .click();

  // Set third column
  cy.get(getElementFromAlias('column-2')).type(getColName(2));
  tableColumnTypeSelector('col-type-2');
  cy.get(getElementFromAlias('data_test_column_type_value_text'))
    .first()
    .click();
  // Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  // Create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${getTableName(0, testName)}/modify`
  );
};

export const passPTCheckRoute = () => {
  // Go to permissiosn tab
  cy.get(getElementFromAlias('table-permissions')).click();
  // Match the URL
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${getTableName(
      0,
      testName
    )}/permissions`
  );
};

export const passPTNoChecks = () => {
  // Type role
  cy.get(getElementFromAlias('role-textbox')).type('role0');
  // Set permissions
  testPermissions(getTableName(0, testName), 'none');
};

export const passPTCustomChecks = () => {
  testPermissions(getTableName(0, testName), 'custom');
};

export const passPTRemovePerms = () => {
  queryTypes.forEach(query => {
    permRemove(getTableName(0, testName), query);
  });
};

export const passPVCreateView = () => {
  // create a view
  createView(getTableName(1, testName), getTableName(0, testName));
  cy.wait(5000);
};

export const passPVPermissions = () => {
  // Track the view
  trackView();
  // Type role
  cy.get(getElementFromAlias('role-textbox')).type('role0');
  // Test permissions
  testPermissions(getTableName(1, testName), 'none', true);
  testPermissions(getTableName(1, testName), 'custom', true);
};

export const passPVRemovePerms = () => {
  permRemove(getTableName(1, testName), 'select');
};

export const passPVDeleteView = () => {
  // Go to modify view
  cy.get(getElementFromAlias('table-modify')).click();
  // Delete view
  setPromptValue(getTableName(1, testName));
  cy.get(getElementFromAlias('delete-view')).click();
  cy.window()
    .its('prompt')
    .should('be.called');
  cy.wait(7000);
};

export const passPTDeleteTable = () => {
  // Go to the table
  cy.get(getElementFromAlias(getTableName(0, testName))).click();
  cy.wait(7000);
  // Go to modify table
  cy.get(getElementFromAlias('table-modify')).click();
  // Delete table
  setPromptValue(getTableName(0, testName));
  cy.get(getElementFromAlias('delete-table')).click();
  cy.window()
    .its('prompt')
    .should('be.called');
  cy.wait(7000);
};

export const setValidationMetaData = () => {
  setMetaData();
};
