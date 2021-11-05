import {
  baseUrl,
  getElementFromAlias,
  getElementFromClassName,
} from '../../helpers/dataHelpers';
import { setPromptValue } from '../../helpers/common';
import { AWAIT_LONG } from '../../helpers/constants';
import { getTimeoutSeconds } from '../../helpers/eventHelpers';

const statements = {
  createMutationActionText: `type Mutation {
    login (username: String!, password: String!): LoginResponse
  }`,
  createMutationCustomType: `type LoginResponse {
    accessToken: String!
  }
  `,
  createMutationHandler: 'https://hasura-actions-demo.glitch.me/login',
  createMutationGQLQuery: `mutation getAccessToken ($username: String!, $password: String!) {
    login (username: $username, password: $password) {
      accessToken
    `,
  createMutationQueryVars: `{"username": "john", "password": "p"`,
  createQueryActionText: `type Query {
    addNumbers (numbers: [Int]): AddResult
  }`,
  createQueryActionCustomType: `type AddResult {
    sum: Int
  }`,
  createQueryHandler: 'https://hasura-actions-demo.glitch.me/addNumbers',
  createQueryGQLQuery: `query {
    addNumbers(numbers: [1, 2, 3, 4]) {
      sum
    `,
  changeHandlerText: 'http://host.docker.internal:3000',
  createActionTransform: `type Mutation {
    login (username: String!, password: String!): LoginResponse
  }
  `,
  createTransformCustomType: `type LoginResponse {
      accessToken: String!
    }
  `,
  createTransformHandler: 'https://hasura-actions-demo.glitch.me/login',
  createTransformIncorrectPayloadBody: `
  {
    "userInfo": {
      "name": {{$input.username}}
  `,
  createTransformPayloadBody: `
  {
    "userInfo": {
      "name": {{$body.input.username}},
      "password": {{$body.input.password}},
      "type": {{$body.action.name}}
  `,
};

// NOTE: This test suite does not include cases for relationships, headers and
// the codegen part

const clearActionDef = () => {
  cy.get('textarea').first().type('{selectall}', { force: true });
  cy.get('textarea').first().trigger('keydown', {
    keyCode: 46,
    which: 46,
    force: true,
  });
};

const clearActionTypes = () => {
  cy.get('textarea').eq(1).type('{selectall}', { force: true });
  cy.get('textarea').eq(1).trigger('keydown', {
    keyCode: 46,
    which: 46,
    force: true,
  });
};

const clearHandler = () => {
  cy.get(
    getElementFromAlias('action-create-handler-input')
  ).type('{selectall}{backspace}', { force: true });
};

const clearRequestUrl = () => {
  cy.get(
    getElementFromAlias('transform-requestUrl')
  ).type('{selectall}{backspace}', { force: true });
};

const clearPayloadTransformBody = () => {
  cy.get('textarea').eq(4).type('{selectall}', { force: true });
  cy.get('textarea').eq(4).trigger('keydown', {
    keyCode: 46,
    which: 46,
    force: true,
  });
};

const typeIntoActionDef = (content: string) => {
  cy.get('textarea').first().type(content, { force: true });
};

const typeIntoActionTypes = (content: string) => {
  cy.get('textarea').eq(1).type(content, { force: true });
};

const typeIntoHandler = (content: string) => {
  cy.get(getElementFromAlias('action-create-handler-input')).type(content, {
    force: true,
  });
};

const clickOnCreateAction = () => {
  cy.get(getElementFromAlias('create-action-btn'))
    .scrollIntoView()
    .click({ force: true });
  cy.get('.notification', { timeout: AWAIT_LONG })
    .should('be.visible')
    .and('contain', 'Created action successfully');
};

const togglePayloadTransformSection = () => {
  cy.get(getElementFromAlias('toggle-payload-transform')).click({
    force: true,
  });
};

const typeIntoTransformBody = (content: string) => {
  cy.get('textarea')
    .eq(4)
    .type(content, { force: true, parseSpecialCharSequences: false });
};

const typeIntoRequestUrl = (content: string) => {
  cy.get(getElementFromAlias('transform-requestUrl')).type(content, {
    parseSpecialCharSequences: false,
  });
};

const typeIntoRequestQueryParams = () => {
  cy.get(getElementFromAlias('transform-kv-key-0')).type('id');
  cy.get(getElementFromAlias('transform-kv-value-0')).type('5');
  cy.get(getElementFromAlias('transform-kv-key-1')).type('name');
  cy.get(
    getElementFromAlias('transform-kv-value-1')
  ).type('{{$body.action.name}}', { parseSpecialCharSequences: false });
};

const checkTransformRequestUrlError = (exists: boolean, error?: string) => {
  if (exists) {
    if (error) {
      cy.get(getElementFromAlias('transform-requestUrl-error'))
        .should('exist')
        .and('contain', error);
    } else {
      cy.get(getElementFromAlias('transform-requestUrl-error')).should('exist');
    }
  } else {
    cy.get(getElementFromAlias('transform-requestUrl-error')).should(
      'not.exist'
    );
  }
};

const checkTransformRequestBodyError = (exists: boolean) => {
  if (exists) {
    cy.get(getElementFromAlias('transform-requestBody-error')).should('exist');
  } else {
    cy.get(getElementFromAlias('transform-requestBody-error')).should(
      'not.exist'
    );
  }
};

const checkTransformRequestUrlPreview = (previewText: string) => {
  cy.get(getElementFromAlias('transform-requestUrl-preview')).should(
    'have.value',
    previewText
  );
};

const toggleRequestTransformSection = () => {
  cy.get(getElementFromAlias('toggle-request-transform')).click({
    force: true,
  });
};

export const routeToGraphiql = () => {
  cy.visit('/api/api-explorer');
  cy.url({ timeout: AWAIT_LONG }).should('eq', `${baseUrl}/api/api-explorer`);
};

export const createMutationAction = () => {
  // Click on create
  cy.get(getElementFromAlias('data-create-actions')).click();
  cy.intercept('*', req => {
    // send all other requests to the destination server
    req.reply();
  });
  // Clear default text on
  clearActionDef();
  // type statement
  typeIntoActionDef(statements.createMutationActionText);
  // clear defaults on action types
  clearActionTypes();
  // type the action type text
  typeIntoActionTypes(statements.createMutationCustomType);
  // clear handler
  clearHandler();
  // type into handler
  typeIntoHandler(statements.createMutationHandler);
  cy.get(getElementFromAlias('action-timeout-seconds'))
    .clear()
    .type(getTimeoutSeconds());
  // click to create action
  clickOnCreateAction();
  cy.get(getElementFromAlias('action-timeout-seconds')).should(
    'have.value',
    '25'
  );
};

export const verifyMutation = () => {
  routeToGraphiql();
  // Type the query
  cy.on('uncaught:exception', () => {
    // NOTE: doing this since, there was some exception thrown by the
    // graphiql editor even though the query was good.
    // Docs: https://docs.cypress.io/api/events/catalog-of-events.html#To-turn-off-all-uncaught-exception-handling
    return false;
  });
  cy.get('textarea')
    .eq(0)
    .type(`{enter}{uparrow}${statements.createMutationGQLQuery}`, {
      force: true,
    });
  cy.get('textarea')
    .eq(1)
    .type(`{enter}{uparrow}${statements.createMutationQueryVars}`, {
      force: true,
    });
  cy.get(getElementFromClassName('execute-button')).click();
  // FIXME: NOT GOOD!
  cy.get('.cm-property').contains('login');
  cy.get('.cm-property').contains('accessToken');
  cy.get('.cm-string').contains('Ew8jkGCNDGAo7p35RV72e0Lk3RGJoJKB');
};

export const modifyMutationAction = () => {
  cy.visit('/actions/manage/login/modify');
  cy.url({ timeout: AWAIT_LONG }).should(
    'eq',
    `${baseUrl}/actions/manage/login/modify`
  );

  clearHandler();
  typeIntoHandler(statements.changeHandlerText);

  cy.get(getElementFromAlias('action-timeout-seconds')).type('{selectall}', {
    force: true,
  });
  cy.get(getElementFromAlias('action-timeout-seconds')).clear().type('50');
  cy.get(getElementFromAlias('save-modify-action-changes')).click();
  cy.get('.notification', { timeout: AWAIT_LONG })
    .should('be.visible')
    .and('contain', 'Action saved successfully');
  cy.get(getElementFromAlias('action-timeout-seconds')).should(
    'have.value',
    '50'
  );

  // permissions part
  cy.get(getElementFromAlias('actions-permissions')).click();

  cy.get(getElementFromAlias('role-textbox')).type('hakuna_matata');

  cy.get(getElementFromAlias('hakuna_matata-Permission')).click();
  cy.get(getElementFromAlias('save-permissions-for-action')).click();

  cy.get(getElementFromAlias('actions-modify')).click();
};

const deleteAction = (promptValue: string) => {
  setPromptValue(promptValue);
  cy.get(getElementFromAlias('delete-action')).click();
  cy.window().its('prompt').should('be.called');
};

export const deleteMutationAction = () => deleteAction('login');

export const createQueryAction = () => {
  // Routing to the index page
  cy.visit('/actions/manage/actions');
  cy.intercept('*', req => {
    // send all other requests to the destination server
    req.reply();
  });
  cy.url({ timeout: AWAIT_LONG }).should(
    'eq',
    `${baseUrl}/actions/manage/actions`
  );
  // Click on create
  cy.get(getElementFromAlias('data-create-actions')).click();
  // Clear default text on
  clearActionDef();
  // type statement
  typeIntoActionDef(statements.createQueryActionText);
  // clear defaults on action types
  clearActionTypes();
  // type the action type text
  typeIntoActionTypes(statements.createQueryActionCustomType);
  // clear handler
  clearHandler();
  // type into handler
  typeIntoHandler(statements.createQueryHandler);
  // click to create action
  clickOnCreateAction();
};

export const verifyQuery = () => {
  cy.on('uncaught:exception', () => {
    // NOTE: doing this since, there was some exception thrown by the
    // graphiql editor even though the query was good.
    // Docs: https://docs.cypress.io/api/events/catalog-of-events.html#To-turn-off-all-uncaught-exception-handling
    return false;
  });
  routeToGraphiql();
  cy.get('textarea')
    .eq(0)
    .type(`{enter}{uparrow}${statements.createQueryGQLQuery}`, { force: true })
    .wait(4000);
  cy.get(getElementFromClassName('execute-button')).click();
  cy.get('.cm-property').contains('addNumbers');
  cy.get('.cm-property').contains('sum');
  cy.get('.cm-number').contains('10');
};

export const modifyQueryAction = () => {
  cy.visit('/actions/manage/addNumbers/modify');
  cy.url({ timeout: AWAIT_LONG }).should(
    'eq',
    `${baseUrl}/actions/manage/addNumbers/modify`
  );

  clearHandler();
  typeIntoHandler(statements.changeHandlerText);

  cy.get(getElementFromAlias('save-modify-action-changes')).click();

  // permissions part
  cy.get(getElementFromAlias('actions-permissions')).click();

  cy.get(getElementFromAlias('role-textbox')).type('MANAGER');

  cy.get(getElementFromAlias('MANAGER-Permission')).click();
  cy.get(getElementFromAlias('save-permissions-for-action')).click();

  cy.get(getElementFromAlias('actions-modify')).click();
};

export const deleteQueryAction = () => deleteAction('addNumbers');

export const createActionTransform = () => {
  // Click on create
  cy.get(getElementFromAlias('data-create-actions')).click();
  // Clear default text on
  clearActionDef();
  // type statement
  typeIntoActionDef(statements.createActionTransform);
  // clear defaults on action types
  clearActionTypes();
  // type the action type text
  typeIntoActionTypes(statements.createTransformCustomType);
  cy.get(getElementFromAlias('action-timeout-seconds'))
    .clear()
    .type(getTimeoutSeconds());
  // open request transform section
  toggleRequestTransformSection();
  cy.get(getElementFromAlias('transform-POST')).click();

  // give correct body without webhook handler
  clearHandler();
  typeIntoRequestUrl('{{$base_url}}');
  cy.wait(1500);
  // check for error
  checkTransformRequestUrlError(
    true,
    'Please configure your webhook handler to generate request url transform'
  );

  // clear handler
  clearHandler();
  // type into handler
  typeIntoHandler(statements.createTransformHandler);

  // give incorrect body
  clearRequestUrl();
  typeIntoRequestUrl('{{$url}}/users');
  cy.wait(1500);
  // check for error
  checkTransformRequestUrlError(true);

  // give correct body
  clearRequestUrl();
  typeIntoRequestUrl('{{$base_url}}');
  typeIntoRequestQueryParams();
  cy.wait(1500);
  // check there is no error
  checkTransformRequestUrlError(false);
  // check the preview is correctly shown
  checkTransformRequestUrlPreview(
    'https://hasura-actions-demo.glitch.me/login?name=login&id=5'
  );

  // open payload transform section
  togglePayloadTransformSection();

  // give incorrect body
  clearPayloadTransformBody();
  typeIntoTransformBody(statements.createTransformIncorrectPayloadBody);
  cy.wait(1500);
  checkTransformRequestBodyError(true);

  // give correct body
  clearPayloadTransformBody();
  typeIntoTransformBody(statements.createTransformPayloadBody);
  cy.wait(1500);
  checkTransformRequestBodyError(false);

  // click to create action
  cy.intercept('*', req => {
    // send all other requests to the destination server
    req.reply();
  });
  clickOnCreateAction();
  cy.get(getElementFromAlias('action-timeout-seconds')).should(
    'have.value',
    '25'
  );
};

export const modifyActionTransform = () => {
  cy.visit('/actions/manage/login/modify');
  cy.url({ timeout: AWAIT_LONG }).should(
    'eq',
    `${baseUrl}/actions/manage/login/modify`
  );
  toggleRequestTransformSection();
  cy.get(getElementFromAlias('transform-GET')).click();
  cy.get(getElementFromAlias('transform-requestUrl'))
    .clear()
    .type('{{$base_url}}/actions', { parseSpecialCharSequences: false });
  cy.get(getElementFromAlias('transform-kv-remove-button-0')).click();
  togglePayloadTransformSection();

  cy.get(getElementFromAlias('save-modify-action-changes')).click();
  cy.get('.notification', { timeout: AWAIT_LONG })
    .should('be.visible')
    .and('contain', 'Action saved successfully');
};

export const deleteActionTransform = () => deleteAction('login');
