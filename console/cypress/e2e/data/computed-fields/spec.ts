import {
  baseUrl,
  getElementFromAlias,
  getElementFromClassName,
} from '../../../helpers/dataHelpers';

const statements = {
  createTableSql:
    'CREATE TABLE a_test_test_author (id serial PRIMARY KEY, first_name text, last_name text);',
  createCustomFuncSql: `CREATE OR REPLACE FUNCTION test_get_author_full_name(a_test_test_author_row a_test_test_author)
    RETURNS TEXT AS $function$
    SELECT a_test_test_author_row.first_name || ' ' || a_test_test_author_row.last_name
    $function$
    LANGUAGE sql STABLE;`,
  insertData_a1: `INSERT INTO a_test_test_author(first_name, last_name) VALUES ('ruskin', 'bond');`,
  insertData_a2: `INSERT INTO a_test_test_author(first_name, last_name) VALUES ('enid', 'blyton');`,
  cleanUpSql: 'DROP TABLE a_test_test_author CASCADE;',
  graphql: {
    query: `{
        a_test_test_author {
          full_name  # this is the computed field`,
  },
};

export const openRawSQL = () => {
  cy.get('a').contains('Data').click();
  cy.wait(3000);
  cy.get(getElementFromAlias('sql-link')).click();
  cy.wait(3000);
  cy.url().should('eq', `${baseUrl}/data/sql`);
};

const clearText = () => {
  cy.get('textarea').type('{selectall}', { force: true });
  cy.get('textarea').trigger('keydown', {
    keyCode: 46,
    which: 46,
    force: true,
  });
  cy.wait(2000);
};

// helper to type into the SQL textarea on rawsql page
const typeStatement = (
  statement: string,
  shouldClearText = false,
  waitTimeUponType = 2000,
  endWaitTime = 5000,
  uncheckTrackCheckbox = false
) => {
  if (shouldClearText) {
    clearText();
  }
  cy.get('textarea').type(statement, { force: true });
  cy.wait(waitTimeUponType);
  if (uncheckTrackCheckbox) {
    cy.get(getElementFromAlias('raw-sql-track-check')).uncheck();
  }
  cy.get(getElementFromAlias('run-sql')).click();
  // FIXME: maybe necessary for CLI mode
  // cy.get(getElementFromAlias('raw-sql-statement-timeout')).should('be.disabled');
  cy.wait(endWaitTime);
};

export const createTableAuthor = () => typeStatement(statements.createTableSql);

export const createCustomFunction = () =>
  typeStatement(statements.createCustomFuncSql, true, 2000, 5000, true);

export const insertAuthorsIntoTable = () => {
  typeStatement(statements.insertData_a1, true);
  typeStatement(statements.insertData_a2, true);
  clearText();
};

export const searchForTable = () => {
  // ADD LATER: after search functionality is implemented
  // cy.get(getElementFromAlias('search-tables')).type('a_test_test_author');
  // cy.get(getElementFromAlias('table-links')).should(
  //   'contain',
  //   'a_test_test_author'
  // );
  cy.get(getElementFromAlias('a_test_test_author')).click();
};

export const openModifySection = () => {
  // open modify section
  cy.get(getElementFromAlias('table-modify')).click();
  // click on computed field section
  cy.get(getElementFromAlias('modify-table-edit-computed-field-0')).click();
  // type name
  cy.get(getElementFromAlias('computed-field-name-input')).type('{selectall}', {
    force: true,
  });
  cy.get(getElementFromAlias('computed-field-name-input')).trigger('keydown', {
    keyCode: 46,
    which: 46,
    force: true,
  });
  cy.get(getElementFromAlias('computed-field-name-input')).type('full_name', {
    force: true,
  });
  cy.wait(2000);
  // type & select function name
  cy.get(getElementFromClassName('function-name-select__control'))
    .children('div')
    .click({ multiple: true })
    .find('input')
    .focus()
    .type('test_get_author_full_name', { force: true })
    .get(getElementFromClassName('function-name-select__menu'))
    .first()
    .click();
  // enter comment
  cy.get(
    getElementFromAlias('computed-field-comment-input')
  ).type('this is a test comment', { force: true });
  // saving the computed field
  cy.get(getElementFromAlias('modify-table-computed-field-0-save')).click();
  // verify that a computed field exists
  cy.wait(5000);
  cy.get(getElementFromAlias('computed-field-full_name')).contains('full_name');
  cy.wait(5000);
};

export const routeToGraphiql = () => {
  cy.visit('/api/api-explorer');
  cy.wait(7000);
  cy.url().should('eq', `${baseUrl}/api/api-explorer`);
};

export const verifyComputedFieldsResult = () => {
  // type the query
  cy.get('textarea')
    .first()
    .type(`{enter}{uparrow}${statements.graphql.query}`, { force: true });
  cy.wait(2000);
  // execute the query
  cy.get('.execute-button').click();
  // verify if full_name is present
  cy.get('.cm-property').contains('full_name');
  cy.get('.cm-string').contains('ruskin bond');
  cy.wait(2000);
};

export const cleanUpSql = () => typeStatement(statements.cleanUpSql, true);

export const routeToSQLPage = () => {
  cy.visit('/data/sql');
  cy.wait(7000);
  cy.url().should('eq', `${baseUrl}/data/sql`);
};
