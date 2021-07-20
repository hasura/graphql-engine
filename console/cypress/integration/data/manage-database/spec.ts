import { setPromptWithCb } from '../../../helpers/common';
import { baseUrl } from '../../../helpers/dataHelpers';

const config = {
  host: 'localhost',
  port: '5432',
  dbName: 'gql_test',
  username: 'gql_test',
  password: '',
};
const dbUrl = `postgres://${config.username}:${config.password}@${config.host}:${config.port}/${config.dbName}`;

export const openManageDatabases = () => {
  cy.getBySel('sidebar-manage-database').click();
  cy.url().should('eq', `${baseUrl}/data/manage`);
};

export const expandAddDatabaseForm = () => {
  cy.get('button').contains('Connect Database').click();
  cy.url().should('eq', `${baseUrl}/data/manage/connect`);
};

export const expandConnectionSettingsform = () => {
  cy.get('a').contains('Connection Settings').click();
};

export const failsOnEmptyFormSubmission = () => {
  cy.getBySel('connect-database-btn').click();
  cy.get('.notification-error').should('be.visible');
};

export const addsNewPostgresDatabaseWithUrl = () => {
  cy.getBySel('database-display-name').type('testDB1');
  cy.getBySel('database-type').select('postgres');
  cy.getBySel('database-url').type(dbUrl);
  cy.getBySel('max-connections').type('50');
  cy.getBySel('idle-timeout').type('180');
  cy.getBySel('retries').type('1');
  cy.getBySel('connect-database-btn').click();
  cy.get('.notification-success', { timeout: 10000 })
    .should('be.visible')
    .and('contain', 'Data source added successfully!');
  cy.url().should('eq', `${baseUrl}/data/manage`);
};

export const addsNewPgDBWithConParams = () => {
  cy.getBySel('sidebar-manage-database').click();
  cy.get('button').contains('Connect Database').click();
  cy.get("input[type='radio']").eq(2).click();
  cy.getBySel('database-display-name').type('testDB2');
  cy.getBySel('database-type').select('postgres');
  cy.getBySel('host').type(config.host);
  cy.getBySel('port').type(config.port);
  cy.getBySel('username').type(config.username);
  if (config.password) {
    cy.getBySel('password').type(config.password);
  }
  cy.getBySel('database-name').type(config.dbName);
  cy.getBySel('connect-database-btn').click();
  cy.get('.notification-success', { timeout: 10000 })
    .should('be.visible')
    .and('contain', 'Data source added successfully!');
  cy.url().should('eq', `${baseUrl}/data/manage`);
};

export const addsNewPgDBWithEnvVar = () => {
  cy.getBySel('sidebar-manage-database').click();
  cy.get('button').contains('Connect Database').click();
  cy.get("input[type='radio']").eq(0).click();
  cy.getBySel('database-display-name').type('testDB3');
  cy.getBySel('database-type').select('postgres');
  cy.getBySel('database-url-env').type('HASURA_GRAPHQL_DATABASE_URL');
  cy.getBySel('connect-database-btn').click();
  cy.get('.notification-success', { timeout: 10000 })
    .should('be.visible')
    .and('contain', 'Data source added successfully!');
  cy.url().should('eq', `${baseUrl}/data/manage`);
};

export const failDuplicateNameDb = () => {
  cy.getBySel('sidebar-manage-database').click();
  cy.get('button').contains('Connect Database').click();
  cy.getBySel('database-display-name').type('testDB1');
  cy.getBySel('database-url').type(dbUrl);
  cy.getBySel('connect-database-btn').click();
  cy.get('.notification-error')
    .should('be.visible')
    .and('contain', 'Adding data source failed');
};

export const deleteTestDBWithUrl = () => {
  cy.getBySel('sidebar-manage-database').click();
  setPromptWithCb('testDB1', () => {
    cy.getBySel('testDB1').find('button').contains('Remove').click();
    cy.get('.notification-success')
      .should('be.visible')
      .and('contain', 'Data source removed successfully');
    cy.window().its('prompt').should('be.called');
  });
};
export const deleteTestDBWithConParams = () => {
  setPromptWithCb('testDB2', () => {
    cy.getBySel('testDB2').find('button').contains('Remove').click();
    cy.get('.notification-success')
      .should('be.visible')
      .and('contain', 'Data source removed successfully');
    cy.window().its('prompt').should('be.called');
  });
};
export const deleteTestDDWithEnvVar = () => {
  setPromptWithCb('testDB3', () => {
    cy.getBySel('testDB3').find('button').contains('Remove').click();
    cy.get('.notification-success')
      .should('be.visible')
      .and('contain', 'Data source removed successfully');
    cy.window().its('prompt').should('be.called');
  });
};
