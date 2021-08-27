import { setPromptWithCb } from '../../../helpers/common';
import { getIndexRoute, baseUrl } from '../../../helpers/dataHelpers';
import { setMetaData } from '../../validators/validators';

export const navigateAndOpenConnectDatabasesForm = () => {
  // visit index route and set metadata
  cy.visit(getIndexRoute()).then(setMetaData);

  // visit the manage database route
  cy.getBySel('sidebar-manage-database').click();
  cy.url().should('eq', `${baseUrl}/data/manage`);

  // open the create database form
  cy.get('button').contains('Connect Database').click();
  cy.url().should('eq', `${baseUrl}/data/manage/connect`);

  // open up the connection settings section of the form so that all <input> elements are visible
  cy.get('a').contains('Connection Settings').click();
};

export const expectNotif = (
  type: string,
  {
    title,
    message,
  }: {
    title: string;
    message?: string;
  },
  timeout = 10000
) => {
  const types: Record<string, string> = {
    error: '.notification-error',
    success: '.notification-success',
  };

  const el = cy.get(types[type], { timeout });
  el.should('be.visible');
  el.should('contain', title);
  if (message) el.should('contain', message);
};

export const navigateToManageDatabases = () => {
  // visit index route and set metadata
  cy.visit(getIndexRoute()).then(setMetaData);

  // visit the manage database route
  cy.getBySel('sidebar-manage-database').click();
  cy.url().should('eq', `${baseUrl}/data/manage`);
};

export const submitConnectDBForm = () => {
  cy.getBySel('connect-database-btn').click();
};

export const removeDBFromList = (dbName: string) => {
  setPromptWithCb(dbName, () => {
    cy.getBySel(dbName).find('button').contains('Remove').click();
  });
};

export const verifyUrl = (url: string) => {
  cy.url().should('eq', url);
};

export type driverSpecType = {
  name: 'postgres' | 'mssql';
  tests: {
    addDBWithURL?: (dbName: string) => void;
    addDBWithConnParams?: (dbName: string) => void;
    addDBWithEnvVar?: (dbName: string) => void;
    removeDBFromList?: (dbName: string) => void;
    addDBWithURLAndExpectDuplicateError?: (dbName: string) => void;
    fillDetailsForEnvVarForm?: (dbName: string) => void;
    fillDetailsForDbUrlForm?: (dbName: string) => void;
    fillDetailsForConnParamsForm?: (dbName: string) => void;
  };
  helpers: {
    createDB: (dbName: string) => void;
    removeDB: (dbName: string) => void;
  };
};
