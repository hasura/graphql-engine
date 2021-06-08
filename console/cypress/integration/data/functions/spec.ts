import {
  getElementFromAlias,
  baseUrl,
  getCustomFunctionName,
  getSchema,
  dropTable,
  testCustomFunctionSQLWithSessArg,
  getTrackFnPayload,
  createFunctionTable,
  trackCreateFunctionTable,
  getCreateTestFunctionQuery,
  getTrackTestFunctionQuery,
  createSampleTable,
  getTrackSampleTableQuery,
  createVolatileFunction,
  dropTableIfExists,
} from '../../../helpers/dataHelpers';

import {
  dataRequest,
  validateCFunc,
  validateUntrackedFunc,
  ResultType,
  trackFunctionRequest,
} from '../../validators/validators';
import { setPromptValue } from '../../../helpers/common';

export const createCustomFunctionSuccess = () => {
  // Round about way to create a function
  dataRequest(createFunctionTable(), ResultType.SUCCESS, 'query');
  cy.wait(5000);
  dataRequest(trackCreateFunctionTable(), ResultType.SUCCESS, 'metadata');
  cy.wait(5000);

  dataRequest(getCreateTestFunctionQuery(1), ResultType.SUCCESS, 'query');
  cy.wait(5000);
  dataRequest(getTrackTestFunctionQuery(1), ResultType.SUCCESS, 'metadata');
  cy.wait(5000);

  // Check if the track checkbox is clicked or not
  validateCFunc(getCustomFunctionName(1), getSchema(), ResultType.SUCCESS);
  cy.wait(5000);
};

export const unTrackFunction = () => {
  cy.visit(
    `data/default/schema/public/functions/${getCustomFunctionName(1)}/modify`
  );
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
  cy.get(
    getElementFromAlias(`add-track-function-${getCustomFunctionName(1)}`)
  ).should('exist');
  cy.get(
    getElementFromAlias(`add-track-function-${getCustomFunctionName(1)}`)
  ).click();
  cy.get(getElementFromAlias(`track-as-mutation`)).should('exist');
  cy.get(getElementFromAlias(`track-as-mutation`)).click();
  cy.wait(5000);
  validateCFunc(getCustomFunctionName(1), getSchema(), ResultType.SUCCESS);
  cy.wait(5000);
};

export const testSessVariable = () => {
  const fN = 'customFunctionWithSessionArg'.toLowerCase();
  dataRequest(
    dropTableIfExists({ name: 'text_result', schema: 'public' }),
    ResultType.SUCCESS
  );
  cy.wait(3000);
  dataRequest(createSampleTable(), ResultType.SUCCESS, 'query');
  cy.wait(3000);
  dataRequest(getTrackSampleTableQuery(), ResultType.SUCCESS, 'metadata');
  cy.wait(3000);

  dataRequest(testCustomFunctionSQLWithSessArg(fN), ResultType.SUCCESS);
  cy.wait(3000);

  trackFunctionRequest(getTrackFnPayload(fN), ResultType.SUCCESS);
  cy.wait(5000);

  cy.visit(`data/default/schema/public/functions/${fN}/modify`);
  cy.get(getElementFromAlias(`${fN}-session-argument-btn`), {
    timeout: 5000,
  }).click();

  // invalid data should fail
  cy.get(getElementFromAlias(`${fN}-edit-sessvar-function-field`))
    .clear()
    .type('invalid');
  cy.get(getElementFromAlias(`${fN}-session-argument-save`)).click();
  cy.get('.notification-error', { timeout: 5000 })
    .should('be.visible')
    .and('contain', 'Updating Session argument variable failed');

  cy.get(getElementFromAlias(`${fN}-session-argument-btn`), {
    timeout: 1000,
  }).click();
  cy.get(getElementFromAlias(`${fN}-edit-sessvar-function-field`))
    .clear()
    .type('hasura_session');
  cy.get(getElementFromAlias(`${fN}-session-argument-save`)).click();
  cy.wait(3000);
  cy.get(getElementFromAlias(fN)).should('be.visible');
  cy.visit(`data/default/schema/public/functions/${fN}/modify`);
  cy.wait(3000);
  cy.get(getElementFromAlias(`${fN}-session-argument`)).should(
    'contain',
    'hasura_session'
  );
  dataRequest(dropTable('text_result', true), ResultType.SUCCESS);
  cy.wait(3000);
  cy.visit(`data/default/schema/public/`);
};

export const verifyPermissionTab = () => {
  cy.get(getElementFromAlias('functions-data/default-permissions')).click();
  cy.wait(5000);
  cy.get(getElementFromAlias('custom-function-permission-link')).should(
    'exist'
  );
  cy.wait(5000);
};

export const deleteCustomFunction = () => {
  cy.get(getElementFromAlias('functions-data/default-modify')).click();

  setPromptValue(getCustomFunctionName(1));

  cy.get(getElementFromAlias('custom-function-edit-delete-btn')).click();
  cy.window().its('prompt').should('be.called');
  cy.wait(5000);
  cy.get(getElementFromAlias('delete-confirmation-error')).should('not.exist');
  cy.url().should('eq', `${baseUrl}/data/default/schema/public`);
  cy.wait(5000);

  dataRequest(dropTable(), ResultType.SUCCESS);
  cy.wait(5000);
};

export const trackVolatileFunction = () => {
  const fN = 'customVolatileFunc'.toLowerCase();
  dataRequest(
    dropTableIfExists({ name: 'text_result', schema: 'public' }),
    ResultType.SUCCESS
  );
  cy.wait(5000);
  dataRequest(createSampleTable(), ResultType.SUCCESS);
  cy.wait(5000);
  dataRequest(getTrackSampleTableQuery(), ResultType.SUCCESS, 'metadata');
  dataRequest(createVolatileFunction(fN), ResultType.SUCCESS);
  cy.wait(5000);
  cy.visit(`data/default/schema/public`);
  cy.get(getElementFromAlias(`add-track-function-${fN}`)).click();
  cy.get(getElementFromAlias('track-as-mutation')).click();
  cy.wait(2000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/functions/${fN}/modify`
  );
  dataRequest(dropTable('text_result', true), ResultType.SUCCESS);
};

export const trackVolatileFunctionAsQuery = () => {
  const fN = 'customVolatileFunc'.toLowerCase();
  dataRequest(
    dropTableIfExists({ name: 'text_result', schema: 'public' }),
    ResultType.SUCCESS
  );
  cy.wait(5000);
  dataRequest(createSampleTable(), ResultType.SUCCESS);
  cy.wait(5000);
  dataRequest(getTrackSampleTableQuery(), ResultType.SUCCESS, 'metadata');
  dataRequest(createVolatileFunction(fN), ResultType.SUCCESS);
  cy.wait(5000);
  cy.visit(`data/default/schema/public`);
  cy.get(getElementFromAlias(`add-track-function-${fN}`)).click();
  cy.get(getElementFromAlias('track-as-query')).click();
  cy.wait(2000);
  cy.get(getElementFromAlias('track-as-query-confirm')).click();
  cy.wait(2000);
  cy.url().should(
    'eq',
    `${baseUrl}/data/default/schema/public/functions/${fN}/modify`
  );
  dataRequest(dropTable('text_result', true), ResultType.SUCCESS);
};
