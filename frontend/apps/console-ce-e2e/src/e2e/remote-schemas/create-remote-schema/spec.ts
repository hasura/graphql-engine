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

  cy.get(getElementFromAlias('data-create-remote-schemas')).click();
  cy.url().should('eq', `${baseUrl}/remote-schemas/manage/add`);
};

export const failRSWithInvalidRemoteUrl = () => {

  cy.get('[data-testid=name]').type(
    getRemoteSchemaName(0, testName)
  );
  cy.get('[data-testid=url]').type(
    getInvalidRemoteSchemaUrl() 
  );
  
  cy.get('[data-testid=submit]').click();
  cy.get('.notifications-wrapper').contains('Error');

};

export const createSimpleRemoteSchema = () => {
  cy.get('[data-testid=name]')
    .clear()
    .type(getRemoteSchemaName(1, testName));
  cy.get('[data-testid=url]')
    .clear()
    .type(getRemoteGraphQLURL());
  cy.get('[data-testid=submit]').click();
  cy.get('.notifications-wrapper').contains('Success');
  validateRS(getRemoteSchemaName(1, testName), ResultType.SUCCESS);
  cy.url().should(
    'eq',
    `${baseUrl}/remote-schemas/manage/${getRemoteSchemaName(
      1,
      testName
    )}/details`
  );
};

export const failRSDuplicateSchemaName = () => {
  cy.visit('remote-schemas/manage/add');
  cy.get('[data-testid=name]')
    .clear()
    .type(getRemoteSchemaName(1, testName));
  cy.get('[data-testid=url]')
    .clear()
    .type(getRemoteGraphQLURL());
  cy.get('[data-testid=submit]').click();
  cy.get('.notifications-wrapper').contains('Error');
  cy.url().should('eq', `${baseUrl}/remote-schemas/manage/add`);
};

export const failRSDuplicateSchemaNodes = () => {
  cy.visit('remote-schemas/manage/add');
  cy.get('[data-testid=name]')
    .clear()
    .type(getRemoteSchemaName(2, testName));
  cy.get('[data-testid=url]')
    .clear()
    .type(getRemoteGraphQLURL());
  cy.get('[data-testid=submit]').click();
  cy.get('.notifications-wrapper').contains('Error');
  cy.url().should('eq', `${baseUrl}/remote-schemas/manage/add`);
};

export const deleteSimpleRemoteSchemaFailUserConfirmationError = () => {
  cy.visit(`remote-schemas/manage/${getRemoteSchemaName(1, testName)}/details`);

  cy.get(getElementFromAlias('remote-schemas-modify')).click();
  setPromptValue(null);
  cy.get(getElementFromAlias('remote-schema-edit-delete-btn')).click();
  cy.window().its('prompt').should('be.called');

  cy.url().should(
    'eq',
    `${baseUrl}/remote-schemas/manage/${getRemoteSchemaName(
      1,
      testName
    )}/modify`
  );
};

export const deleteSimpleRemoteSchema = () => {
  cy.visit(`remote-schemas/manage/${getRemoteSchemaName(1, testName)}/details`);

  cy.get(getElementFromAlias('remote-schemas-modify')).click();
  setPromptValue(getRemoteSchemaName(1, testName));
  cy.get(getElementFromAlias('remote-schema-edit-delete-btn')).click();
  cy.window().its('prompt').should('be.called');
  cy.get(getElementFromAlias('delete-confirmation-error')).should('not.exist');
};

export const failWithRemoteSchemaEnvUrl = () => {
  cy.visit('remote-schemas/manage/add');
  cy.get('[data-testid=name]')
    .clear()
    .type(getRemoteSchemaName(3, testName));
  cy.get(
    '[name="url.type"]'
  ).select('from_env');
  cy.get('[data-testid=url]')
    .clear()
    .type(getRemoteGraphQLURLFromEnv());
  cy.get('[data-testid=submit]').click();
  cy.get('.notifications-wrapper').contains('Error');
  cy.url().should('eq', `${baseUrl}/remote-schemas/manage/add`);
};

export const failWithRemoteSchemaEnvHeader = () => {
  cy.visit('remote-schemas/manage/add');
  cy.get('[data-testid=name]')
    .clear()
    .type(getRemoteSchemaName(3, testName));
  cy.get('[data-testid=url]')
    .clear()
    .type(getRemoteGraphQLURL());

  cy.get('[data-testid="add-header')
    .click()

  cy.get('[name="headers[0].name"]')
    .clear()
    .type('sampleHeader1');

  cy.get('[name="headers[0].value"]')
    .clear()
    .type('sampleHeaderValue1');

  cy.get('[data-testid="add-header')
    .click()

  cy.get('[name="headers[1].name"]')
    .clear()
    .type('sampleHeader2');

  cy.get(
    '[name="headers[1].type"]'
  ).select("from_env");

  cy.get('[name="headers[1].value"]')
    .clear()
    .type('SAMPLE_ENV_HEADER');

  cy.get('[data-testid=submit]').click();
  cy.get('.notifications-wrapper').contains('Error');
  cy.url().should('eq', `${baseUrl}/remote-schemas/manage/add`);
};

export const passWithRemoteSchemaHeader = () => {
  cy.visit('remote-schemas/manage/add');
  cy.get('[data-testid=name]')
    .clear()
    .type(getRemoteSchemaName(3, testName));
  cy.get('[data-testid=url]')
    .clear()
    .type(getRemoteGraphQLURL());

  cy.get('[data-testid="add-header')
    .click()

  cy.get('[name="headers[0].name"]')
    .clear()
    .type('sampleHeader1');

  cy.get('[name="headers[0].value"]')
    .clear()
    .type('sampleHeaderValue1');

  cy.get('[data-testid="add-header')
    .click()

  cy.get('[name="headers[1].name"]')
    .clear()
    .type('sampleHeader2');

  cy.get('[name="headers[1].value"]')
    .clear()
    .type('sampleHeaderValue2');

  cy.get('[data-testid=submit]').click();
  cy.get('.notifications-wrapper').contains('Success');
  validateRS(getRemoteSchemaName(3, testName), ResultType.SUCCESS);
  cy.url().should(
    'eq',
    `${baseUrl}/remote-schemas/manage/${getRemoteSchemaName(
      3,
      testName
    )}/details`
  );
};

export const deleteRemoteSchema = () => {
  cy.visit(`remote-schemas/manage/${getRemoteSchemaName(3, testName)}/details`);

  cy.get(getElementFromAlias('remote-schemas-modify')).click();
  setPromptValue(getRemoteSchemaName(3, testName));
  cy.get(getElementFromAlias('remote-schema-edit-delete-btn')).click();
  cy.window().its('prompt').should('be.called');
  cy.get(getElementFromAlias('delete-confirmation-error')).should('not.exist');
};

export const visitRemoteSchemaPermissionsTab = () => {
  cy.visit(
    `${baseUrl}/remote-schemas/manage/${getRemoteSchemaName(
      1,
      testName
    )}/permissions`
  );
};

export const createSimpleRemoteSchemaPermission = () => {
  cy.get(getElementFromAlias('role-textbox'))
    .clear()
    .type(getRemoteSchemaRoleName(1, testName));
  cy.get(
    getElementFromAlias(`${getRemoteSchemaRoleName(1, testName)}-Permission`)
  ).click();
  cy.get(getElementFromAlias('field-__query_root')).click();
  cy.get(getElementFromAlias('checkbox-query')).click();
  cy.get(getElementFromAlias('save-remote-schema-permissions')).click({
    force: true,
  });
  cy.get('.notifications-wrapper').contains('saved')
  cy.url().should(
    'eq',
    `${baseUrl}/remote-schemas/manage/${getRemoteSchemaName(
      1,
      testName
    )}/permissions`
  );
  cy.get(getElementFromAlias('role-test-role-rs-1')).should('be.visible');
};

export const passWithUpdateRemoteSchema = () => {
  cy.visit(
    `${baseUrl}/remote-schemas/manage/${getRemoteSchemaName(
      3,
      testName
    )}/modify`
  );
  cy.get(getElementFromAlias('remote-schema-schema-name')).should(
    'have.attr',
    'disabled'
  );
  cy.get(getElementFromAlias('remote-schema-comment'))
    .clear()
    .type('This is a new remote schema comment');

  cy.get(getElementFromAlias('remote-schema-edit-save-btn')).click();
  cy.get('.notifications-wrapper').contains('modified');
  validateRS(getRemoteSchemaName(3, testName), ResultType.SUCCESS);

  cy.get(getElementFromAlias('remote-schemas-modify')).click();
  cy.get(getElementFromAlias('remote-schema-schema-name')).should(
    'have.attr',
    'value',
    getRemoteSchemaName(3, testName)
  );
};
