import {
  getElementFromAlias,
  baseUrl,
  getRemoteSchemaName,
  getInvalidRemoteSchemaUrl,
  getRemoteGraphQLURL,
  getRemoteGraphQLURLFromEnv,
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
  cy.wait(5000);
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
  cy.wait(5000);
};

export const createSimpleRemoteSchema = () => {
  cy.get(getElementFromAlias('remote-schema-schema-name'))
    .clear()
    .type(getRemoteSchemaName(1, testName));
  cy.get(getElementFromAlias('remote-schema-graphql-url-input'))
    .clear()
    .type(getRemoteGraphQLURL());
  cy.get(getElementFromAlias('add-remote-schema-submit')).click();
  cy.wait(10000);
  validateRS(getRemoteSchemaName(1, testName), 'success');
  cy.url().should(
    'eq',
    `${baseUrl}/remote-schemas/manage/${getRemoteSchemaName(
      1,
      testName
    )}/details`
  );
  cy.wait(5000);
};

export const failRSDuplicateSchemaName = () => {
  cy.visit('remote-schemas/manage/add');
  cy.get(getElementFromAlias('remote-schema-schema-name'))
    .clear()
    .type(getRemoteSchemaName(1, testName));
  cy.get(getElementFromAlias('remote-schema-graphql-url-input'))
    .clear()
    .type(getRemoteGraphQLURL());
  cy.get(getElementFromAlias('add-remote-schema-submit')).click();
  cy.wait(5000);
  cy.url().should('eq', `${baseUrl}/remote-schemas/manage/add`);
  cy.wait(5000);
};

export const failRSDuplicateSchemaNodes = () => {
  cy.visit('remote-schemas/manage/add');
  cy.get(getElementFromAlias('remote-schema-schema-name'))
    .clear()
    .type(getRemoteSchemaName(2, testName));
  cy.get(getElementFromAlias('remote-schema-graphql-url-input'))
    .clear()
    .type(getRemoteGraphQLURL());
  cy.get(getElementFromAlias('add-remote-schema-submit')).click();
  cy.wait(5000);
  cy.url().should('eq', `${baseUrl}/remote-schemas/manage/add`);
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
  cy.wait(5000);
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
  cy.wait(5000);
};

export const failWithRemoteSchemaEnvUrl = () => {
  cy.visit('remote-schemas/manage/add');
  cy.get(getElementFromAlias('remote-schema-schema-name'))
    .clear()
    .type(getRemoteSchemaName(3, testName));
  cy.get(
    getElementFromAlias('remote-schema-graphql-url-dropdown-button')
  ).click();
  cy.get(
    getElementFromAlias('remote-schema-graphql-url-dropdown-item-2')
  ).click();
  cy.get(getElementFromAlias('remote-schema-graphql-url-input'))
    .clear()
    .type(getRemoteGraphQLURLFromEnv());
  cy.get(getElementFromAlias('add-remote-schema-submit')).click();
  cy.wait(5000);
  cy.url().should('eq', `${baseUrl}/remote-schemas/manage/add`);
  cy.wait(5000);
};

export const failWithRemoteSchemaEnvHeader = () => {
  cy.visit('remote-schemas/manage/add');
  cy.get(getElementFromAlias('remote-schema-schema-name'))
    .clear()
    .type(getRemoteSchemaName(3, testName));
  cy.get(getElementFromAlias('remote-schema-graphql-url-input'))
    .clear()
    .type(getRemoteGraphQLURL());

  cy.get(getElementFromAlias('remote-schema-header-test1-key'))
    .clear()
    .type('sampleHeader1');

  cy.get(getElementFromAlias('remote-schema-header-test1-input'))
    .clear()
    .type('sampleHeaderValue1');

  cy.get(getElementFromAlias('remote-schema-header-test2-key'))
    .clear()
    .type('sampleHeader2');

  cy.get(
    getElementFromAlias('remote-schema-header-test2-dropdown-button')
  ).click();

  cy.get(
    getElementFromAlias('remote-schema-header-test2-dropdown-item-2')
  ).click();

  cy.get(getElementFromAlias('remote-schema-header-test2-input'))
    .clear()
    .type('SAMPLE_ENV_HEADER');
  cy.get(getElementFromAlias('add-remote-schema-submit')).click();
  cy.wait(5000);
  cy.url().should('eq', `${baseUrl}/remote-schemas/manage/add`);
  cy.wait(5000);
};

export const passWithRemoteSchemaHeader = () => {
  cy.visit('remote-schemas/manage/add');
  cy.get(getElementFromAlias('remote-schema-schema-name'))
    .clear()
    .type(getRemoteSchemaName(3, testName));
  cy.get(getElementFromAlias('remote-schema-graphql-url-input'))
    .clear()
    .type(getRemoteGraphQLURL());

  cy.get(getElementFromAlias('remote-schema-header-test1-key'))
    .clear()
    .type('sampleHeader1');

  cy.get(getElementFromAlias('remote-schema-header-test1-input'))
    .clear()
    .type('sampleHeaderValue1');

  cy.get(getElementFromAlias('remote-schema-header-test2-key'))
    .clear()
    .type('sampleHeader2');

  cy.get(getElementFromAlias('remote-schema-header-test2-input'))
    .clear()
    .type('sampleHeaderValue2');

  cy.get(getElementFromAlias('add-remote-schema-submit')).click();
  cy.wait(5000);
  validateRS(getRemoteSchemaName(3, testName), 'success');
  cy.url().should(
    'eq',
    `${baseUrl}/remote-schemas/manage/${getRemoteSchemaName(
      3,
      testName
    )}/details`
  );
  cy.wait(5000);
};

export const passWithEditRemoteSchema = () => {
  cy.visit(
    `${baseUrl}/remote-schemas/manage/${getRemoteSchemaName(
      3,
      testName
    )}/modify`
  );
  cy.wait(3000);
  cy.get(getElementFromAlias('remote-schema-edit-modify-btn'))
    .should('exist')
    .click();
  cy.get(getElementFromAlias('remote-schema-schema-name'))
    .clear()
    .type(getRemoteSchemaName(5, testName));

  cy.get(getElementFromAlias('remote-schema-edit-save-btn')).click();
  cy.wait(5000);
  validateRS(getRemoteSchemaName(5, testName), 'success');

  cy.get(getElementFromAlias('remote-schemas-modify')).click();
  cy.get(getElementFromAlias('remote-schema-schema-name')).should(
    'have.attr',
    'value',
    getRemoteSchemaName(5, testName)
  );
  cy.get(getElementFromAlias('remote-schema-edit-modify-btn')).should('exist');
  cy.wait(5000);
};

export const deleteRemoteSchema = () => {
  cy.visit(
    `remote-schemas/manage/${getRemoteSchemaName(5, testName)}/details`,
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
