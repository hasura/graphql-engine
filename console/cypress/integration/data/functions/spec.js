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
  // Are you absolutely sure?\nThis action cannot be undone. This will permanently delete stitched GraphQL schema. Please type "DELETE" (in caps, without quotes) to confirm.\n
  cy.visit(`data/schema/public/functions/${getCustomFunctionName(1)}/modify`);
  cy.wait(5000);
  cy.get(getElementFromAlias('custom-function-edit-untrack-btn')).click();
  cy.wait(5000);
  validateUntrackedFunc(getCustomFunctionName(1), getSchema(), 'success');
  cy.wait(5000);
};

export const trackFunction = () => {
  // Are you absolutely sure?\nThis action cannot be undone. This will permanently delete stitched GraphQL schema. Please type "DELETE" (in caps, without quotes) to confirm.\n
  /*
  cy.visit(
    `data/schema/public/functions/${getCustomFunctionName(1)}/modify`,
  );
  */
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
  // Are you absolutely sure?\nThis action cannot be undone. This will permanently delete stitched GraphQL schema. Please type "DELETE" (in caps, without quotes) to confirm.\n
  cy.visit(
    `data/schema/public/functions/${getCustomFunctionName(1)}/permissions`
  );
  cy.wait(5000);
  cy.get(getElementFromAlias('custom-function-permission-link')).should(
    'exist'
  );
  cy.wait(5000);
};

export const deleteCustomFunction = () => {
  // Are you absolutely sure?\nThis action cannot be undone. This will permanently delete stitched GraphQL schema. Please type "DELETE" (in caps, without quotes) to confirm.\n
  cy.visit(`data/schema/public/functions/${getCustomFunctionName(1)}/modify`, {
    onBeforeLoad(win) {
      cy.stub(win, 'prompt').returns('DELETE');
    },
  });

  cy.wait(5000);

  cy.get(getElementFromAlias('custom-function-edit-delete-btn')).click();
  cy.wait(5000);
  cy.window()
    .its('prompt')
    .should('be.called');
  cy.get(getElementFromAlias('delete-confirmation-error')).should('not.exist');
  cy.url().should('eq', `${baseUrl}/data/schema/public`);
  cy.wait(5000);

  dropTableRequest(dropTable(1), 'success');
  cy.wait(5000);
};
