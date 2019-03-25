import {
  getElementFromAlias,
  getTableName,
  getColName,
  baseUrl,
} from '../../../helpers/dataHelpers';
import { setMetaData, validateCT } from '../../validators/validators';

const testName = 'ct';

export const checkCreateTableRoute = () => {
  //    Click on the create table button
  cy.visit('/data/schema');
  cy.wait(15000);
  cy.get(getElementFromAlias('data-create-table')).click();
  //   Match the URL
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
};

export const failCTWithoutColumns = () => {
  //    Type table name
  cy.get(getElementFromAlias('tableName')).type(getTableName(0, testName));
  //    Click on create
  cy.get(getElementFromAlias('table-create')).click();
  //    Check for an error
  // cy.get('div').contains('Column name cannot be empty');
  // cy.get('.notification-error');
  //    Check if the route didn't change
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  //   Validate
  validateCT(getTableName(0, testName), 'failure');
};

export const failCTWithoutPK = () => {
  //   Set first column
  cy.get(getElementFromAlias('column-0')).type(getColName(0));
  cy.get(getElementFromAlias('col-type-0')).select('serial');
  //   Click on create
  cy.get(getElementFromAlias('table-create')).click();
  //   Check for an error
  // cy.get('.notification-error');
  //   Check if the route didn't change
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  //   Validate
  validateCT(getTableName(0, testName), 'failure');
};

export const failCTDuplicateColumns = () => {
  //   Set second column
  cy.get(getElementFromAlias('column-1')).type(getColName(0));
  cy.get(getElementFromAlias('col-type-1')).select('serial');
  //   Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  //   Click on create
  cy.get(getElementFromAlias('table-create')).click();
  //   Check for an alert
  cy.on('window:alert', str => {
    expect(
      str === `You have the following column names repeated: [${getColName(0)}]`
    ).to.be.true;
  });
  //   Check if the route didn't change
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  //   Validate
  validateCT(getTableName(0, testName), 'failure');
};

export const failCTWrongDefaultValue = () => {
  //   Set second column
  cy.get(getElementFromAlias('column-1'))
    .clear()
    .type(getColName(1));
  cy.get(getElementFromAlias('col-type-1')).select('integer');
  cy.get(getElementFromAlias('col-default-1')).type('qwerty');
  //   Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  //   Click on create
  cy.get(getElementFromAlias('table-create')).click();
  //   Check if the route didn't change
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  //   Validate
  validateCT(getTableName(0, testName), 'failure');
};

export const passCT = () => {
  //   Set second column
  cy.get(getElementFromAlias('column-1'))
    .clear()
    .type(getColName(1));
  cy.get(getElementFromAlias('col-type-1')).select('text');
  cy.get(getElementFromAlias('col-default-1')).clear();
  //   Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  cy.get(getElementFromAlias('primary-key-select-1')).select('1');
  //  Click on create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(10000);
  //  Check if the table got created and navigatied to modify table
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0, testName)}/modify`
  );
  cy.get(getElementFromAlias(getTableName(0, testName)));
  //   Validate
  validateCT(getTableName(0, testName), 'success');
};

export const passCTWithFK = () => {
  // go to create-table
  cy.get(getElementFromAlias('sidebar-add-table')).click();
  // Set tablename
  cy.get(getElementFromAlias('tableName'))
    .clear()
    .type(getTableName(1, testName)); //   Set first column
  cy.get(getElementFromAlias('column-0')).type(getColName(0));
  cy.get(getElementFromAlias('col-type-0')).select('serial');
  // Set second column
  cy.get(getElementFromAlias('column-1')).type(getColName(1));
  cy.get(getElementFromAlias('col-type-1')).select('text');
  //   Set primary key
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
  //  Click on create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(10000);
  //  Check if the table got created and navigatied to modify table
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(1, testName)}/modify`
  );
  cy.get(getElementFromAlias(getTableName(1, testName)));
  //   Validate
  validateCT(getTableName(1, testName), 'success');
};

export const failCTDuplicateTable = () => {
  //  Visit data page
  cy.get(getElementFromAlias('sidebar-add-table')).click();
  //  Type table name
  cy.get(getElementFromAlias('tableName')).type(getTableName(0, testName));
  //   Set column
  cy.get(getElementFromAlias('column-0')).type(getColName(1));
  cy.get(getElementFromAlias('col-type-0')).select('serial');
  //   Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  //  Click on create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
  //  Detect error
  // cy.get('.notification-error');
};

export const deleteCTTestTable = () => {
  //   Go to the modify section of the second table
  cy.get(getElementFromAlias(`${getTableName(1, testName)}`)).click();
  cy.get(getElementFromAlias('table-modify')).click();
  //   Click on delete
  cy.get(getElementFromAlias('delete-table')).click();
  //   Confirm
  cy.on('window:confirm', str => {
    expect(str === 'Are you sure?').to.be.true;
    return true;
  });
  cy.wait(5000);
  validateCT(getTableName(1, testName), 'failure');
  //   Go to the modify section of the first table
  cy.get(getElementFromAlias(`${getTableName(0, testName)}`)).click();
  cy.get(getElementFromAlias('table-modify')).click();
  //   Click on delete
  cy.get(getElementFromAlias('delete-table')).click();
  //   Confirm
  cy.on('window:confirm', str => {
    expect(str === 'Are you sure?').to.be.true;
    return true;
  });
  cy.wait(5000);
  //Match the URL
  cy.url().should('eq', `${baseUrl}/data/schema/public`);
  //   Validate
  validateCT(getTableName(0, testName), 'failure');
};

export const setValidationMetaData = () => {
  setMetaData();
};
