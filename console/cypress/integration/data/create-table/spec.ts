import {
  tableColumnTypeSelector,
  getElementFromAlias,
  getTableName,
  getColName,
  baseUrl,
  getIndexRoute,
} from '../../../helpers/dataHelpers';
import {
  setMetaData,
  validateCT,
  ResultType,
} from '../../validators/validators';
import { setPromptValue } from '../../../helpers/common';

const testName = 'ct';

export const checkCreateTableRoute = () => {
  // Click on the create table button
  cy.visit(getIndexRoute());
  cy.wait(15000);
  cy.get(getElementFromAlias('data-create-table')).click();
  // Match the URL
  cy.url().should('eq', `${baseUrl}/data/default/schema/public/table/add`);
};

export const failCTWithoutColumns = () => {
  // Type table name
  cy.get(getElementFromAlias('tableName')).type(getTableName(0, testName));
  // Click on create
  cy.get(getElementFromAlias('table-create')).click();
  // Check if the route didn't change
  cy.url().should('eq', `${baseUrl}/data/default/schema/public/table/add`);
  // Validate
  validateCT(getTableName(0, testName), ResultType.FAILURE);
};

export const failCTWithoutPK = () => {
  // Set first column
  cy.get(getElementFromAlias('column-0')).type(getColName(0));
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_serial'))
    .first()
    .click();
  // Click on create
  cy.get(getElementFromAlias('table-create')).click();
  // Check if the route didn't change
  cy.url().should('eq', `${baseUrl}/data/default/schema/public/table/add`);
  // Validate
  validateCT(getTableName(0, testName), ResultType.FAILURE);
};

export const failCTDuplicateColumns = () => {
  // Set second column
  cy.get(getElementFromAlias('column-1')).type(getColName(0));
  tableColumnTypeSelector('col-type-1');
  cy.get(getElementFromAlias('data_test_column_type_value_serial'))
    .first()
    .click();
  // Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  // Click on create
  cy.get(getElementFromAlias('table-create')).click();
  // Check for an alert
  cy.on('window:alert', str => {
    expect(
      str === `You have the following column names repeated: [${getColName(0)}]`
    ).to.be.true;
  });
  // Check if the route didn't change
  cy.url().should('eq', `${baseUrl}/data/default/schema/public/table/add`);
  // Validate
  validateCT(getTableName(0, testName), ResultType.FAILURE);
};

export const failCTWrongDefaultValue = () => {
  // Set second column
  cy.get(getElementFromAlias('column-1')).clear().type(getColName(1));
  tableColumnTypeSelector('col-type-1');
  cy.get(getElementFromAlias('data_test_column_type_value_integer'))
    .first()
    .click();
  cy.get(getElementFromAlias('col-default-1')).type('qwerty');
  // Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  // Click on create
  cy.get(getElementFromAlias('table-create')).click();
  // Check if the route didn't change
  cy.url().should('eq', `${baseUrl}/data/default/schema/public/table/add`);
  // Validate
  validateCT(getTableName(0, testName), ResultType.FAILURE);
};

export const passCT = () => {
  // Set second column
  cy.get(getElementFromAlias('column-1')).clear().type(getColName(1));
  tableColumnTypeSelector('col-type-1');
  cy.get(getElementFromAlias('data_test_column_type_value_text'))
    .first()
    .click();
  cy.get(getElementFromAlias('col-default-1')).clear();
  // Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  cy.get(getElementFromAlias('primary-key-select-1')).select('1');
  // Click on create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(10000);
  // Check if the table got created and navigatied to modify table
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${getTableName(0, testName)}/modify`
  );
  cy.get(getElementFromAlias(getTableName(0, testName)));
  // Validate
  validateCT(getTableName(0, testName), ResultType.SUCCESS);
};

export const passCTWithFK = () => {
  // go to create-table
  cy.get(getElementFromAlias('sidebar-add-table')).click();
  // Set tablename
  cy.get(getElementFromAlias('tableName'))
    .clear()
    .type(getTableName(1, testName));
  // Set first column
  cy.get(getElementFromAlias('column-0')).type(getColName(0));
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_serial'))
    .first()
    .click();
  // Set second column
  cy.get(getElementFromAlias('column-1')).type(getColName(1));
  tableColumnTypeSelector('col-type-1');
  cy.get(getElementFromAlias('data_test_column_type_value_text'))
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
  // Set foreign key
  cy.get(getElementFromAlias('add-table-edit-fk-0')).click();
  cy.get(getElementFromAlias('foreign-key-ref-table-0')).select(
    getTableName(0, testName)
  );
  cy.get(getElementFromAlias('foreign-key-0-lcol-0')).select('0');
  cy.get(getElementFromAlias('foreign-key-0-rcol-0')).select(getColName(0));
  cy.get(getElementFromAlias('foreign-key-0-lcol-1')).select('1');
  cy.get(getElementFromAlias('foreign-key-0-rcol-1')).select(getColName(1));
  cy.get(getElementFromAlias('foreign-key-0-onUpdate-cascade')).check();
  cy.get(getElementFromAlias('foreign-key-0-onDelete-cascade')).check();

  // set unique key 1
  cy.get(getElementFromAlias('add-table-edit-unique-key-0')).click();
  cy.get(getElementFromAlias('unique-key-0-column-0')).select('1');

  // set unique key 2
  cy.get(getElementFromAlias('add-table-edit-unique-key-1')).click();
  cy.get(getElementFromAlias('unique-key-1-column-0')).select('1');
  cy.get(getElementFromAlias('unique-key-1-column-1')).select('2');
  cy.get(getElementFromAlias('unique-key-1-column-2')).select('0');
  cy.get(getElementFromAlias('remove-uk-1-column-1')).click();

  // Click on create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(10000);
  // Check if the table got created and navigatied to modify table
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/tables/${getTableName(1, testName)}/modify`
  );
  cy.get('div').contains(
    `${getTableName(1, testName)}_${getColName(1)}_${getColName(0)}`
  );
  cy.get(getElementFromAlias(getTableName(1, testName)));
  // Validate
  validateCT(getTableName(1, testName), ResultType.SUCCESS);
};

export const failCTDuplicateTable = () => {
  // Visit data page
  cy.get(getElementFromAlias('sidebar-add-table')).click();
  // Type table name
  cy.get(getElementFromAlias('tableName')).type(getTableName(0, testName));
  // Set column
  cy.get(getElementFromAlias('column-0')).type(getColName(1));
  tableColumnTypeSelector('col-type-0');
  cy.get(getElementFromAlias('data_test_column_type_value_serial'))
    .first()
    .click();
  // Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  // Click on create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
};

const deleteTable = (tableName: string) => {
  cy.get(getElementFromAlias(tableName)).click();
  cy.get(getElementFromAlias('table-modify')).click();

  setPromptValue(tableName);

  // Click on delete
  cy.get(getElementFromAlias('delete-table')).click();
  // Confirm
  cy.window().its('prompt').should('be.called');

  cy.wait(5000);
  validateCT(tableName, ResultType.FAILURE);
};

export const deleteCTTestTables = () => {
  // Go to the modify section of the second table
  const secondTableName = getTableName(1, testName);
  deleteTable(secondTableName);
  // Go to the modify section of the first table
  const firstTableName = getTableName(0, testName);
  deleteTable(firstTableName);

  // Match the URL

  // FIXME: Temporarily disabling this.
  // cy.url().should('eq', `${baseUrl}/data/schema`);
};

export const setValidationMetaData = () => {
  setMetaData();
};
