import {
  getElementFromAlias,
  baseUrl,
  // testCustomFunctionDefinition,
  getCustomFunctionName,
  getSchema,
  testCustomFunctionSQL,
  createTable,
  dropTable,
} from '../../../helpers/dataHelpers';

import {
  dropTableRequest,
  dataRequest,
  validateCFunc,
  validateUntrackedFunc,
} from '../../validators/validators';
import { setPromptValue } from '../../../helpers/common';

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

export const createCustomFunctionSuccess = () => {
  // cy.get('textarea').type(testCustomFunctionDefinition(1), { timeout: 10000, force: true});
  // Round about way to create a function
  dataRequest(createTable(1), 'success');
  cy.wait(5000);
  dataRequest(testCustomFunctionSQL(1), 'success');
  cy.wait(5000);
  // cy.get(getElementFromAlias('run-sql')).click();
  // Check if the track checkbox is clicked or not
  validateCFunc(getCustomFunctionName(1), getSchema(), 'success');
  cy.wait(5000);
};

export const unTrackFunction = () => {
  cy.visit(`data/schema/public/functions/${getCustomFunctionName(1)}/modify`);
  cy.wait(5000);
  cy.get(getElementFromAlias('custom-function-edit-untrack-btn')).click();
  cy.wait(5000);
  validateUntrackedFunc(getCustomFunctionName(1), getSchema(), 'success');
  cy.wait(5000);
};

export const trackFunction = () => {
  cy.get(getElementFromAlias('toggle-trackable-functions')).click();
  cy.get(
    getElementFromAlias(`add-track-function-${getCustomFunctionName(1)}`)
  ).should('exist');
  cy.get(
    getElementFromAlias(`add-track-function-${getCustomFunctionName(1)}`)
  ).click();
  cy.wait(5000);
  validateCFunc(getCustomFunctionName(1), getSchema(), 'success');
  cy.wait(5000);
};

export const verifyPermissionTab = () => {
  cy.get(getElementFromAlias('functions-data-permissions')).click();
  cy.wait(5000);
  cy.get(getElementFromAlias('custom-function-permission-link')).should(
    'exist'
  );
  cy.wait(5000);
};

export const deleteCustomFunction = () => {
  cy.get(getElementFromAlias('functions-data-modify')).click();

  setPromptValue(getCustomFunctionName(1));

  cy.get(getElementFromAlias('custom-function-edit-delete-btn')).click();
  cy.window()
    .its('prompt')
    .should('be.called');
  cy.wait(5000);
  cy.get(getElementFromAlias('delete-confirmation-error')).should('not.exist');
  cy.url().should('eq', `${baseUrl}/data/schema/public`);
  cy.wait(5000);

  dropTableRequest(dropTable(1), 'success');
  cy.wait(5000);
};
