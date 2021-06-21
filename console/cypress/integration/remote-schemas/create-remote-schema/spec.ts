import {
  getElementFromAlias,
  baseUrl,
  getRemoteSchemaName,
  getInvalidRemoteSchemaUrl,
  getRemoteGraphQLURL,
  getRemoteGraphQLURLFromEnv,
  getRemoteSchemaRoleName,
} from '../../../helpers/remoteSchemaHelpers';

import { validateRS, ResultType } from '../../validators/validators';
import { setPromptValue } from '../../../helpers/common';

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

  validateRS(getRemoteSchemaName(0, testName), ResultType.FAILURE);
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
  cy.wait(15000);
  validateRS(getRemoteSchemaName(1, testName), ResultType.SUCCESS);
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
  cy.wait(10000);
};

export const deleteSimpleRemoteSchemaFailUserConfirmationError = () => {
  cy.visit(`remote-schemas/manage/${getRemoteSchemaName(1, testName)}/details`);

  cy.get(getElementFromAlias('remote-schemas-modify')).click();
  cy.wait(5000);
  setPromptValue(null);
  cy.get(getElementFromAlias('remote-schema-edit-delete-btn')).click();
  cy.wait(5000);
  cy.window().its('prompt').should('be.called');

  cy.url().should(
    'eq',
    `${baseUrl}/remote-schemas/manage/${getRemoteSchemaName(
      1,
      testName
    )}/modify`
  );
  // cy.get(getElementFromAlias('delete-confirmation-error')).should('exist');
  cy.wait(5000);
};

export const deleteSimpleRemoteSchema = () => {
  cy.visit(`remote-schemas/manage/${getRemoteSchemaName(1, testName)}/details`);

  cy.get(getElementFromAlias('remote-schemas-modify')).click();
  cy.wait(5000);
  setPromptValue(getRemoteSchemaName(1, testName));
  cy.get(getElementFromAlias('remote-schema-edit-delete-btn')).click();
  cy.window().its('prompt').should('be.called');
  cy.wait(5000);
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
  cy.wait(15000);
  validateRS(getRemoteSchemaName(3, testName), ResultType.SUCCESS);
  cy.url().should(
    'eq',
    `${baseUrl}/remote-schemas/manage/${getRemoteSchemaName(
      3,
      testName
    )}/details`
  );
  cy.wait(5000);
};

export const deleteRemoteSchema = () => {
  cy.visit(`remote-schemas/manage/${getRemoteSchemaName(3, testName)}/details`);

  cy.get(getElementFromAlias('remote-schemas-modify')).click();
  cy.wait(5000);
  setPromptValue(getRemoteSchemaName(3, testName));
  cy.get(getElementFromAlias('remote-schema-edit-delete-btn')).click();
  cy.window().its('prompt').should('be.called');
  cy.wait(5000);

  cy.get(getElementFromAlias('delete-confirmation-error')).should('not.exist');
};

export const visitRemoteSchemaPermissionsTab = () => {
  cy.visit(
    `${baseUrl}/remote-schemas/manage/${getRemoteSchemaName(
      1,
      testName
    )}/permissions`
  );
  cy.wait(5000);
};

export const createSimpleRemoteSchemaPermission = () => {
  cy.get(getElementFromAlias('role-textbox'))
    .clear()
    .type(getRemoteSchemaRoleName(1, testName));
  cy.get(
    getElementFromAlias(`${getRemoteSchemaRoleName(1, testName)}-Permission`)
  ).click();
  cy.wait(2000);
  cy.get(getElementFromAlias('field-__query_root')).click();
  cy.get(getElementFromAlias('checkbox-test')).click();
  cy.get(getElementFromAlias('pen-limit')).click();
  cy.get(getElementFromAlias('input-limit')).type('1');

  cy.get(getElementFromAlias('save-remote-schema-permissions')).click({
    force: true,
  });
  cy.wait(15000);
  cy.url().should(
    'eq',
    `${baseUrl}/remote-schemas/manage/${getRemoteSchemaName(
      1,
      testName
    )}/permissions`
  );
  cy.get(getElementFromAlias('role-test-role-rs-1')).should('be.visible');
  cy.wait(5000);
};

export const passWithUpdateRemoteSchema = () => {
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
  cy.get(getElementFromAlias('remote-schema-schema-name')).should(
    'have.attr',
    'disabled'
  );
  cy.get(getElementFromAlias('remote-schema-comment'))
    .clear()
    .type("This is a new remote schema comment");

  cy.get(getElementFromAlias('remote-schema-edit-save-btn')).click();
  cy.wait(5000);
  validateRS(getRemoteSchemaName(3, testName), ResultType.SUCCESS);

  cy.get(getElementFromAlias('remote-schemas-modify')).click();
  cy.get(getElementFromAlias('remote-schema-schema-name')).should(
    'have.attr',
    'value',
    getRemoteSchemaName(3, testName)
  );
  cy.get(getElementFromAlias('remote-schema-edit-modify-btn')).should('exist');
  cy.wait(7000);
};
