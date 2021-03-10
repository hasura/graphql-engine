import { baseUrl, getElementFromAlias } from '../../../helpers/dataHelpers';

const statements = {
  createTableSql:
    'CREATE TABLE a_test_test_article (id serial PRIMARY KEY, title text, content text);',
  createCustomFuncSql: `CREATE FUNCTION a_test_test_search_articles(search text)
RETURNS SETOF a_test_test_article AS $function$
SELECT *
FROM a_test_test_article
WHERE
title ilike ('%' || search || '%')
OR content ilike ('%' || search || '%')
$function$ LANGUAGE sql STABLE;`,
  insertData_a1: `INSERT INTO a_test_test_article(title, content) VALUES ('hasura is awesome', 'I mean duh?!');`,
  insertData_a2: `INSERT INTO a_test_test_article(title, content) VALUES ('cloud lauched', 'hasura <3 the cloud');`,
  deleteFunction: 'DROP FUNCTION a_test_test_search_articles(search text);',
  cleanUpSql: 'DROP TABLE a_test_test_article CASCADE;',
  graphql: {
    query: `{
        a_test_test_search_articles
          (args: {{} search: "hasura" }) {
            id
            title
            content
        `,
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
  unCheckTrackFunction = false
) => {
  if (shouldClearText) {
    clearText();
  }
  cy.get('textarea').type(statement, { force: true });
  cy.wait(waitTimeUponType);
  if (unCheckTrackFunction) {
    cy.get(getElementFromAlias('raw-sql-track-check')).uncheck();
  }
  cy.get(getElementFromAlias('run-sql')).click();
  // FIXME: maybe necessary for CLI mode
  // cy.get(getElementFromAlias('raw-sql-statement-timeout')).should('be.disabled');
  cy.wait(endWaitTime);
};

export const createTableArticle = () =>
  typeStatement(statements.createTableSql);

export const createCustomFunction = () =>
  typeStatement(statements.createCustomFuncSql, true, 2000, 5000, true);

export const insertAuthorsIntoTable = () => {
  typeStatement(statements.insertData_a1, true);
  typeStatement(statements.insertData_a2, true);
  clearText();
};

export const trackCustomFn = () => {
  cy.visit('/data/default/schema/public');
  cy.wait(7000);
  cy.url().should('eq', `${baseUrl}/data/default/schema/public`);

  // Track Function
  cy.get(
    getElementFromAlias('add-track-function-a_test_test_search_articles')
  ).click();
  cy.wait(5000);
};

export const routeToGraphiql = () => {
  cy.visit('/api/api-explorer');
  cy.wait(7000);
  cy.url().should('eq', `${baseUrl}/api/api-explorer`);
};

export const verifyCustomFnResult = () => {
  // Type the query
  cy.get('textarea')
    .first()
    .type(`{enter}{uparrow}${statements.graphql.query}`, { force: true });
  cy.wait(2000);
  cy.get('.execute-button').click();
  // verify if article is present

  cy.get('.cm-property').contains('title');
  cy.get('.cm-property').contains('content');

  cy.wait(2000);
};

export const cleanUpSql = () => {
  typeStatement(statements.deleteFunction, true);
  typeStatement(statements.cleanUpSql, true);
  clearText();
  cy.wait(2000);
};

export const routeToSQLPage = () => {
  cy.visit('/data/sql');
  cy.wait(7000);
  cy.url().should('eq', `${baseUrl}/data/sql`);
};
