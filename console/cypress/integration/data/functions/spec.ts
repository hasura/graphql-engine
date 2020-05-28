import {
  getElementFromAlias,
  baseUrl,
  getCustomFunctionName,
  getSchema,
  testCustomFunctionSQL,
  createTable,
  dropTable,
  createTableSessVar,
  testCustomFunctionSQLWithSessArg,
  getTrackFnPayload,
} from '../../../helpers/dataHelpers';

import {
  dropTableRequest,
  dataRequest,
  validateCFunc,
  validateUntrackedFunc,
  ResultType,
  createFunctionRequest,
  trackFunctionRequest,
} from '../../validators/validators';
import { setPromptValue } from '../../../helpers/common';

export const createCustomFunctionSuccess = () => {
  // Round about way to create a function
  dataRequest(createTable(), ResultType.SUCCESS);
  cy.wait(5000);
  dataRequest(testCustomFunctionSQL(1), ResultType.SUCCESS);
  cy.wait(5000);
  // Check if the track checkbox is clicked or not
  validateCFunc(getCustomFunctionName(1), getSchema(), ResultType.SUCCESS);
  cy.wait(5000);
};

export const unTrackFunction = () => {
  cy.visit(`data/schema/public/functions/${getCustomFunctionName(1)}/modify`);
  cy.wait(5000);
  cy.get(getElementFromAlias('custom-function-edit-untrack-btn')).click();
  cy.wait(5000);
  validateUntrackedFunc(
    getCustomFunctionName(1),
    getSchema(),
    ResultType.SUCCESS
  );
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
  validateCFunc(getCustomFunctionName(1), getSchema(), ResultType.SUCCESS);
  cy.wait(5000);
};
export const testSessVariable = () => {
  // Round about way to create a function
  const fN = 'customFunctionWithSessionArg'.toLowerCase(); // for reading
  dataRequest(createTableSessVar(), ResultType.SUCCESS);
  createFunctionRequest(
    testCustomFunctionSQLWithSessArg(fN),
    ResultType.SUCCESS
  );
  cy.wait(1500);
  trackFunctionRequest(getTrackFnPayload(fN), ResultType.SUCCESS);
  cy.wait(1500);
  cy.visit(`data/schema/public/functions/${fN}/modify`);
  cy.wait(1500);
  cy.get(getElementFromAlias(`${fN}-session-argument-btn`)).click();
  cy.wait(1000);
  cy.get(getElementFromAlias(`${fN}-edit-sessvar-function-field`))
    .clear()
    .type('hasura_session');
  cy.wait(1000);
  cy.get(getElementFromAlias(`${fN}-session-argument-save`)).click();
  cy.wait(2000);
  cy.get(getElementFromAlias(fN)).should('be.visible');
  cy.visit(`data/schema/public/functions/${fN}/modify`);
  cy.wait(3000);
  cy.get(getElementFromAlias(`${fN}-session-argument`)).should(
    'contain',
    'hasura_session'
  );
  dropTableRequest(dropTable('text_result', true), ResultType.SUCCESS);
  cy.wait(2000);
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

  dropTableRequest(dropTable(), ResultType.SUCCESS);
  cy.wait(5000);
};
