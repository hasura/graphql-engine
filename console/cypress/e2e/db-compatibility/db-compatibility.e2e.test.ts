/**
 * This e2e test is intended to be run manually to verify if a new database is compatible — not to run on the CI
 *
 * NOTE: You need a local cypress.env.json file with some Env Variables set to make it running (connection strings).
 *
 * Example of cypress.env.json content:
 * {
 *    "POSTGRES_DB_CONNECTION_STRING": "postgres://postgres:postgrespassword@postgres:5432/postgres",
 * }
 */

import { Driver } from '../../../src/dataSources';

// NOTE: table name cannot start with number
const getRandomString = () => `a${Math.random().toString(36).slice(3)}`;

const connectionString = Cypress.env('POSTGRES_DB_CONNECTION_STRING');
const driverSelectLabel = 'Postgres';
const driverSelectValue: Driver = 'postgres';
const dataSourceDisplayName = getRandomString();
const tableName = getRandomString();
const consolePort = '3000';

describe.skip('Test Basic Data Source Compatibility', () => {
  it(
    'tests Connect Data Source',
    {
      defaultCommandTimeout: 20000,
    },
    () => {
      cy.visit(`http://localhost:${consolePort}/data/manage/connect`);

      cy.log('/** Type the data source name **/');
      cy.get('[data-test=database-display-name]').click();

      cy.get('[data-test=database-display-name]').type(dataSourceDisplayName);

      cy.log('/** Select the Driver **/');

      cy.get('[data-test=database-type]')
        .select(driverSelectLabel, { force: true })
        .should('have.value', driverSelectValue);

      cy.log('/** Type the connection string **/');
      cy.get('[data-test=database-url]').click();

      cy.get('[data-test=database-url]').type(connectionString);

      cy.log('/** Click connect **/');
      cy.get('[data-test=connect-database-btn] > span').click();

      cy.log('/** Expect to see a success notification **/');
      cy.wait(5000);
      cy.get('.notification-success.notification-visible').should(
        'contain',
        'Data source added successfully!'
      );

      cy.get('.notification-success').should('be.visible');
    }
  );

  it(
    'tests create table',
    {
      defaultCommandTimeout: 20000,
    },
    () => {
      cy.visit(`http://localhost:${consolePort}/data/manage`);

      cy.log('/** Click on the database in the Nav Tree **/');
      cy.contains('Data Manager')
        .parent('li')
        .within(() => {
          cy.findByText(dataSourceDisplayName).click();
        });

      cy.log('/** Click on schema in the Nav Tree **/');
      cy.get('[data-test=table-links]').within(() => {
        cy.findByText('public').click({ force: true });
      });

      cy.log('/** Click on create table **/');
      cy.get('[data-test=data-create-table]').click();

      cy.log('/** Type the table name **/');
      cy.get('[data-test=tableName]').type(tableName);

      cy.log('/** Click on the table comment input **/');
      cy.get('[data-test=tableComment]').click();

      cy.log('/** Type the table comment **/');
      cy.get('[data-test=tableComment]').type('My table comment');

      cy.log('/** Click on column name **/');
      cy.get('[data-test=column-0]').click();

      cy.log('/** Click on "Frequently used columns" **/');
      cy.get(
        '[data-test=frequently-used-columns] > div > div > button > span'
      ).click();

      cy.log('/** Select "id" **/');
      cy.get(
        '[data-test=frequently-used-columns] > div > ul > li:nth-child(1) > button > div > div:nth-child(1)'
      ).click();

      cy.log('/** Click on the second column name **/');
      cy.get('[data-test=column-1]').click();

      cy.log('/** Type column name "name" **/');
      cy.get('[data-test=column-1]').type('name');

      cy.log('/** Click on the second column column_type **/');
      cy.get(
        '[data-test=col-type-1] > div > div > div.css-1hwfws3.col-type-1.add_table_column_selector__value-container'
      ).click();

      cy.log('/** Type text **/');
      cy.get('#react-select-3-input').type('text');

      cy.log('/** Select option "Text" **/');
      cy.get('#react-select-3-option-1-2').click();

      cy.log('/** Click "Create table" **/');
      cy.get('[data-test=table-create] > span').click();

      cy.wait(5000);
      cy.get('.notification-success').should('be.visible');
    }
  );

  it(
    'tests GraphQL Mutation',
    {
      defaultCommandTimeout: 20000,
    },
    () => {
      cy.visit(
        `http://localhost:${consolePort}/data/${dataSourceDisplayName}/schema/public`
      );

      cy.log('/** Click API tab **/');
      cy.get('[data-test=api-tab-link] > p').click();

      cy.log('/** Click GraphiQL Query input field **/');
      cy.get(
        '#apiRequestBlock > div > div > div > div > div.graphiql-container > div.editorWrap > div.editorBar > div.queryWrap > section.query-editor > div > div.CodeMirror-scroll > div.CodeMirror-sizer'
      ).click();

      cy.log('/** Select all and delete **/');
      cy.get('body').type('{cmd}a');
      cy.get('body').type('{del}');

      cy.log('/** Click GraphiQL query input **/');
      cy.get(
        '#apiRequestBlock > div > div > div > div > div.graphiql-container > div.editorWrap > div.editorBar > div.queryWrap > section.query-editor > div > div.CodeMirror-scroll'
      ).click();

      cy.log('/** Select Mutation **/');
      cy.get(
        '#apiRequestBlock > div > div > div > div > div.gqlexplorer > div.docExplorerWrap > div.doc-explorer-contents > div > div:nth-child(2) > form > select'
      ).select('mutation');

      cy.log('/** Click "+" **/');
      cy.get(
        '#apiRequestBlock > div > div > div > div > div.gqlexplorer > div.docExplorerWrap > div.doc-explorer-contents > div > div:nth-child(2) > form > button > span'
      ).click();

      cy.log(`/** Click "insert_${tableName}_one" **/`);
      cy.get(
        `#mutation-MyMutation > div.graphiql-explorer-node.graphiql-explorer-insert_${tableName}_one > span > span.graphiql-explorer-field-view`
      ).click();

      cy.log('/** Click "name" **/');
      cy.get(
        `#mutation-MyMutation > div.graphiql-explorer-insert_${tableName}_one > div.graphiql-explorer-node.graphiql-explorer-insert_${tableName}_one > div > div.graphiql-explorer-object > div > div.graphiql-explorer-name > span:nth-child(1) > svg`
      ).click();

      cy.log('/** Click "name" mutation parameter **/');
      cy.get(
        '#apiRequestBlock > div > div > div > div > div.graphiql-container > div.editorWrap > div.editorBar > div.queryWrap > section.query-editor > div > div.CodeMirror-scroll > div.CodeMirror-sizer > div > div > div > div.CodeMirror-code > div:nth-child(2) > pre > span > span.cm-string'
      ).click();

      cy.log('/** Type name input parameter **/');
      cy.get(
        '#apiRequestBlock > div > div > div > div > div.graphiql-container > div.editorWrap > div.editorBar > div.queryWrap > section.query-editor > div > div:nth-child(1) > textarea'
      ).type('john doe');

      cy.log('/** Click execute **/');
      cy.get(
        '#apiRequestBlock > div > div > div > div > div.graphiql-container > div.editorWrap > div.topBarWrap > div > div.execute-button-wrap > button'
      ).click();

      cy.get(
        '#apiRequestBlock > div > div > div > div > div.graphiql-container > div.editorWrap > div.editorBar > div.resultWrap > section > div > div.CodeMirror-scroll > div.CodeMirror-sizer > div > div > div > div.CodeMirror-code'
      )
        .should('contain.text', '"data": {')
        .should('contain.text', `"insert_${tableName}_one": {`)
        .should('contain.text', '"id":');
    }
  );

  it(
    'tests GraphQL Query',
    {
      defaultCommandTimeout: 20000,
    },
    () => {
      cy.visit(
        `http://localhost:${consolePort}/data/${dataSourceDisplayName}/schema/public`
      );

      cy.log('/** Click API tab **/');
      cy.get('[data-test=api-tab-link] > p').click();

      cy.log('/** Click GraphiQL Query input field **/');
      cy.get(
        '#apiRequestBlock > div > div > div > div > div.graphiql-container > div.editorWrap > div.editorBar > div.queryWrap > section.query-editor > div > div.CodeMirror-scroll > div.CodeMirror-sizer'
      ).click();

      cy.log('/** Select all and delete **/');
      cy.get('body').type('{cmd}a');
      cy.get('body').type('{del}');

      cy.log('/** Click "+" **/');
      cy.get(
        '#apiRequestBlock > div > div > div > div > div.gqlexplorer > div.docExplorerWrap > div.doc-explorer-contents > div > div:nth-child(2) > form > button > span'
      ).click();

      cy.log(`/** Expand ${tableName} **/`);
      cy.get(
        `#query-MyQuery > div.graphiql-explorer-node.graphiql-explorer-${tableName} > span > span:nth-child(1) > svg > path`
      ).click();

      cy.log('/** Click "id" **/');
      cy.get(
        `#query-MyQuery > div.graphiql-explorer-${tableName} > div:nth-child(2) > div.graphiql-explorer-node.graphiql-explorer-id > span > svg`
      ).click();

      cy.log('/** Click "name" **/');
      cy.get(
        `#query-MyQuery > div.graphiql-explorer-${tableName} > div:nth-child(2) > div.graphiql-explorer-node.graphiql-explorer-name > span > svg`
      ).click();

      cy.log('/** Click execute **/');
      cy.get(
        '#apiRequestBlock > div > div > div > div > div.graphiql-container > div.editorWrap > div.topBarWrap > div > div.execute-button-wrap > button'
      ).click();

      cy.get(
        '#apiRequestBlock > div > div > div > div > div.graphiql-container > div.editorWrap > div.editorBar > div.resultWrap > section > div > div.CodeMirror-scroll > div.CodeMirror-sizer > div > div > div > div.CodeMirror-code'
      )
        .should('contain.text', '"data": {')
        .should('contain.text', `"${tableName}": [`)
        .should('contain.text', '"id":');
    }
  );

  it(
    'deletes table',
    {
      defaultCommandTimeout: 20000,
    },
    () => {
      cy.visit(
        `http://localhost:${consolePort}/data/${dataSourceDisplayName}/schema/public/tables/${tableName}/modify`,
        {
          onBeforeLoad(win) {
            cy.stub(win, 'prompt').returns(tableName);
          },
        }
      );

      cy.log('/** Click table in the Nav Tree **/');
      cy.get(`[data-test=${tableName}]`).click();

      cy.log('/** Click Modify tab **/');
      cy.get('[data-test=table-modify]').click();

      cy.log('/** Click Delete **/');
      cy.get('[data-test=delete-table] > span').click();

      cy.window().its('prompt').should('be.called');

      cy.get('.notification-success').should('be.visible');
    }
  );

  it(
    'deletes a data source',
    {
      defaultCommandTimeout: 20000,
    },
    () => {
      cy.visit(`http://localhost:${consolePort}/data/manage`, {
        onBeforeLoad(win) {
          cy.stub(win, 'prompt').returns(dataSourceDisplayName);
        },
      });

      cy.log('/** Click "Manage" **/');
      cy.get('[data-test=sidebar-manage-database]').click();

      cy.log('/** Click "Remove" Data Source **/');
      cy.get(`[data-test="remove-${dataSourceDisplayName}"]`).click({
        force: true,
      });

      cy.window().its('prompt').should('be.called');

      cy.get('.notification-success').should('be.visible');
    }
  );
});
