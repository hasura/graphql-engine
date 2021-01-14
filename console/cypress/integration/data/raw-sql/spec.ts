import { baseUrl, getElementFromAlias } from '../../../helpers/dataHelpers';

let prevStr = '';

export const openRawSQL = () => {
  // Open RawSQL
  cy.get('a').contains('Data').click();
  cy.wait(3000);
  cy.get(getElementFromAlias('sql-link')).click();
  cy.wait(3000);
  // Match URL
  cy.url().should('eq', `${baseUrl}/data/default/sql`);
};
const clearText = () => {
  cy.get('textarea').type('{selectall}', { force: true });
  cy.get('textarea').trigger('keydown', {
    keyCode: 46,
    which: 46,
    force: true,
  });
  cy.wait(2000); // ace editor textarea doesn't expose the value to check, so wait
};

export const passCreateTable = () => {
  prevStr = 'CREATE TABLE Apic_test_table_rsql (id serial PRIMARY KEY);';
  cy.get('textarea').type(prevStr, { force: true });
  cy.wait(1000); // debounce
  cy.get(getElementFromAlias('run-sql')).click();
  cy.get(getElementFromAlias('raw-sql-statement-timeout')).should(
    'be.disabled'
  );
  cy.wait(5000);
};

export const passInsertValues = () => {
  clearText();
  prevStr = 'INSERT INTO Apic_test_table_rsql VALUES (1);';
  cy.get('textarea').type(prevStr, { force: true });
  cy.wait(1000);
  cy.get(getElementFromAlias('run-sql')).click();
  cy.wait(5000);
};

export const passAlterTable = () => {
  clearText();
  prevStr = 'ALTER TABLE Apic_test_table_rsql ADD COLUMN name text;';
  cy.get('textarea').type(prevStr, { force: true });
  // Untrack table
  cy.wait(1000);
  cy.get(getElementFromAlias('raw-sql-track-check')).uncheck();
  cy.get(getElementFromAlias('run-sql')).click();
  cy.wait(5000);
};

export const passCreateView = () => {
  clearText();
  prevStr = 'CREATE VIEW abcd AS SELECT * FROM Apic_test_table_rsql;';
  cy.get('textarea').type(prevStr, { force: true });
  // Track table
  cy.wait(1000);
  cy.get(getElementFromAlias('raw-sql-track-check')).check();
  cy.get(getElementFromAlias('run-sql')).click();
  cy.wait(5000);
};

export const delTestTables = () => {
  clearText();
  prevStr = 'DROP TABLE Apic_test_table_rsql CASCADE;';
  cy.get('textarea').type(prevStr, { force: true });
  cy.wait(1000);
  // cy.get(getElementFromAlias('raw-sql-migration-check')).uncheck();
  cy.get(getElementFromAlias('run-sql')).click();
  // NOTE: This is only visible, when the console is in CLI mode
  // cy.get(getElementFromAlias('not-migration-confirm')).click();
  cy.get(getElementFromAlias('raw-sql-statement-timeout')).type('20', {
    force: true,
  });
  cy.wait(5000);
};
