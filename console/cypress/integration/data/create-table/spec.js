import {
  getElementFromAlias,
  getTableName,
  getColName,
  baseUrl,
} from '../../../helpers/dataHelpers';
import { setMetaData, validateCT } from '../../validators/validators';

export const checkCreateTableRoute = () => {
  //    Click on the create table button
  cy.get('button')
    .contains('Create Table')
    .click();
  //   Match the URL
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
};

export const failCTWithoutColumns = () => {
  //    Type table name
  cy.get(getElementFromAlias('tableName')).type(getTableName(0));
  //    Click on create
  cy.get('button')
    .contains('Create')
    .click();
  //    Check for an error
  cy.get('div').contains('Column name cannot be empty');
  //    Check if the route didn't change
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  //   Validate
  validateCT(getTableName(0), 'failure');
};

export const failCTWithoutPK = () => {
  //   Set first column
  cy.get(getElementFromAlias('column-0')).type(getColName(0));
  cy.get(getElementFromAlias('col-type-0')).select('serial');
  //   Click on create
  cy.get('button')
    .contains('Create')
    .click();
  //   Check for an error
  cy.get('div').contains(
    'You should have atleast one column as a primary key.'
  );
  //   Check if the route didn't change
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  //   Validate
  validateCT(getTableName(0), 'failure');
};

export const failCTDuplicateColumns = () => {
  //   Set second column
  cy.get(getElementFromAlias('column-1')).type(getColName(0));
  cy.get(getElementFromAlias('col-type-1')).select('serial');
  //   Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  //   Click on create
  cy.get('button')
    .contains('Create')
    .click();
  //   Check for an alert
  cy.on('window:alert', str => {
    expect(
      str === `You have the following column names repeated: [${getColName(0)}]`
    ).to.be.true;
  });
  //   Check if the route didn't change
  cy.url().should('eq', `${baseUrl}/data/schema/public/table/add`);
  //   Validate
  validateCT(getTableName(0), 'failure');
};

export const passCT = () => {
  //   Set second column
  cy.get(getElementFromAlias('column-1'))
    .clear()
    .type(getColName(1));
  cy.get(getElementFromAlias('col-type-1')).select('serial');
  //   Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  //  Click on create
  cy.get(getElementFromAlias('table-create')).click();
  cy.wait(7000);
  //  Check if the table got created and navigatied to modify table
  cy.url().should(
    'eq',
    `${baseUrl}/data/schema/public/tables/${getTableName(0)}/modify`
  );
  cy.get(getElementFromAlias(getTableName(0)));
  //   Validate
  validateCT(getTableName(0), 'success');
};

export const failCTDuplicateTable = () => {
  //  Visit data page
  cy.get('button')
    .contains('Add Table')
    .click();
  //  Type table name
  cy.get(getElementFromAlias('tableName')).type(getTableName(0));
  //   Set column
  cy.get(getElementFromAlias('column-0')).type(getColName(1));
  cy.get(getElementFromAlias('col-type-0')).select('serial');
  //   Set primary key
  cy.get(getElementFromAlias('primary-key-select-0')).select('0');
  //  Click on create
  cy.get('button')
    .contains('Create')
    .click();
  cy.wait(5000);
  //  Detect error
  cy.get('div').contains('Create table failed');
};

export const deleteCTTestTable = () => {
  //   Go to the modify section of the table
  cy.get('a')
    .contains(`${getTableName(0)}`)
    .click();
  cy.get(getElementFromAlias('table-modify')).click();
  //   Click on delete
  cy.get('button')
    .contains('Delete table')
    .click();
  //   Confirm
  cy.on('window:confirm', str => {
    expect(str === 'Are you sure?').to.be.true;
    return true;
  });
  cy.wait(7000);
  //   Match the URL
  cy.url().should('eq', `${baseUrl}/data/schema/public`);
  //   Validate
  validateCT(getTableName(0), 'failure');
};

export const setValidationMetaData = () => {
  setMetaData();
};
