/* eslint import/prefer-default-export: 0 */

import { baseUrl } from '../../../helpers/dataHelpers';
import { getElementFromAlias } from '../../../helpers/dataHelpers';
// import { validateCT } from '../../validators/validators';

let prevStr = '';

export const openRawSQL = () => {
  // eslint-disable-line
  // Open RawSQL
  cy.get('a')
    .contains('Data')
    .click();
  cy.wait(3000);
  cy.get(getElementFromAlias('sql-link')).click();
  cy.wait(3000);
  // Match URL
  cy.url().should('eq', `${baseUrl}/data/sql`);
};

export const passCreateTable = () => {
  prevStr = 'CREATE TABLE Apic_test_table_rsql (id serial PRIMARY KEY);';
  cy.get('textarea').type(prevStr, { force: true });
  cy.get(getElementFromAlias('run-sql')).click();
  cy.wait(5000);
  // validateCT('Apic_test_table_rsql', 'success');
};

export const passInsertValues = () => {
  for (let i = 0; i < prevStr.length; i++) {
    cy.get('textarea').type('{backspace}', { force: true });
  }
  prevStr = 'INSERT INTO Apic_test_table_rsql VALUES (1);';
  cy.get('textarea').type(prevStr, { force: true });
  cy.get(getElementFromAlias('run-sql')).click();
  cy.wait(5000);
  // validateCT('Apic_test_table_rsql', 'success');
};

export const passAlterTable = () => {
  for (let i = 0; i < prevStr.length; i++) {
    cy.get('textarea').type('{backspace}', { force: true });
  }
  prevStr = 'ALTER TABLE Apic_test_table_rsql ADD COLUMN name text;';
  cy.get('textarea').type(prevStr, { force: true });
  // Untrack table
  cy.get(getElementFromAlias('raw-sql-track-check')).uncheck();
  cy.get(getElementFromAlias('run-sql')).click();
  cy.wait(5000);
  // validateCT('Apic_test_table_rsql', 'success');
};

export const passCreateView = () => {
  for (let i = 0; i < prevStr.length; i++) {
    cy.get('textarea').type('{backspace}', { force: true });
  }
  prevStr = 'CREATE VIEW abcd AS SELECT * FROM Apic_test_table_rsql;';
  cy.get('textarea').type(prevStr, { force: true });
  // Track table
  cy.get(getElementFromAlias('raw-sql-track-check')).check();
  cy.get(getElementFromAlias('run-sql')).click();
  cy.wait(5000);
  // validateCT('Apic_test_table_rsql', 'success');
};

export const delTestTables = () => {
  for (let i = 0; i < prevStr.length; i++) {
    cy.get('textarea').type('{backspace}', { force: true });
  }
  prevStr = 'DROP TABLE Apic_test_table_rsql CASCADE;';
  cy.get('textarea').type(prevStr, { force: true });
  cy.get(getElementFromAlias('raw-sql-migration-check')).uncheck();
  cy.get(getElementFromAlias('run-sql')).click();
  cy.get(getElementFromAlias('not-migration-confirm')).click();
  cy.wait(5000);
  for (let i = 0; i < prevStr.length; i++) {
    cy.get('textarea').type('{backspace}', { force: true });
  }
  prevStr = 'DROP TABLE abcd;';
  cy.get('textarea').type(prevStr, { force: true });
  cy.get(getElementFromAlias('run-sql')).click();
  cy.wait(5000);
  // cy.visit(`${baseUrl}/data/schema/public`);
  // cy.get(getElementFromAlias('add-track-table-Apic_test_table_rsql')).click();
  // cy.get(getElementFromAlias('delete-table')).click();
  // cy.on('window:confirm', () => true);
  // cy.wait(5000);
  // validateCT('Apic_test_table_rsql', 'failure');
};
