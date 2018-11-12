import {
  getElementFromAlias,
  baseUrl,
  getRemoteSchemaName,
  getInvalidRemoteSchemaUrl,
  getRemoteGraphQLURL,
} from '../../../helpers/remoteSchemaHelpers';

import { validateRS } from '../../validators/validators';

const testName = 'rs';

export const checkCreateRemoteSchemaRoute = () => {
  cy.visit('/remote-schemas/manage/schemas', {
    onBeforeLoad(win) {
      cy.stub(win, 'prompt').returns('DELETE');
    },
  });

  cy.wait(2000);
  cy.get(getElementFromAlias('data-create-remote-schemas')).click();
  cy.url().should('eq', `${baseUrl}/remote-schemas/manage/add`);
};

export const failRSWithInvalidRemoteUrl = () => {
  cy.get(getElementFromAlias('remote-schema-schema-name')).type(
    getRemoteSchemaName(0, testName)
  );
  cy.get(getElementFromAlias('remote-schema-graphql-url-input')).type(
    getInvalidRemoteSchemaUrl()
  );

  cy.get(getElementFromAlias('add-remote-schema-submit')).click();

  validateRS(getRemoteSchemaName(0, testName), 'failure');
  cy.wait(3000);
};

export const createSimpleRemoteSchema = () => {
  cy.get(getElementFromAlias('remote-schema-schema-name'))
    .clear()
    .type(getRemoteSchemaName(1, testName));
  cy.get(getElementFromAlias('remote-schema-graphql-url-input'))
    .clear()
    .type(getRemoteGraphQLURL());
  cy.wait(5000);
  cy.get(getElementFromAlias('add-remote-schema-submit')).click();
  cy.wait(10000);
  validateRS(getRemoteSchemaName(1, testName), 'success');
  cy.wait(5000);
};

export const deleteSimpleRemoteSchemaFailUserConfirmationError = () => {
  cy.visit(
    `remote-schemas/manage/${getRemoteSchemaName(1, testName)}/details`,
    {
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('InvalidInput');
      },
    }
  );

  cy.get(getElementFromAlias('remote-schemas-modify')).click();
  cy.wait(5000);
  cy.get(getElementFromAlias('remote-schema-edit-delete-btn')).click();
  cy.wait(5000);
  cy.window()
    .its('prompt')
    .should('be.called');

  cy.get(getElementFromAlias('delete-confirmation-error')).should('exist');
};

export const deleteSimpleRemoteSchema = () => {
  // Are you absolutely sure?\nThis action cannot be undone. This will permanently delete stitched GraphQL schema. Please type "DELETE" (in caps, without quotes) to confirm.\n
  cy.visit(
    `remote-schemas/manage/${getRemoteSchemaName(1, testName)}/details`,
    {
      onBeforeLoad(win) {
        cy.stub(win, 'prompt').returns('DELETE');
      },
    }
  );

  cy.get(getElementFromAlias('remote-schemas-modify')).click();
  cy.wait(5000);
  cy.get(getElementFromAlias('remote-schema-edit-delete-btn')).click();
  cy.wait(5000);
  cy.window()
    .its('prompt')
    .should('be.called');
  cy.get(getElementFromAlias('delete-confirmation-error')).should('not.exist');
};
