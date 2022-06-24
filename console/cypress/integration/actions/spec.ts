import { baseUrl, getElementFromClassName } from '../../helpers/dataHelpers';
import { setPromptValue } from '../../helpers/common';
import { AWAIT_LONG, AWAIT_SHORT } from '../../helpers/constants';
import { getTimeoutSeconds } from '../../helpers/eventHelpers';
import {
  toggleRequestTransformSection,
  togglePayloadTransformSection,
  typeIntoRequestQueryParams,
  typeIntoRequestUrl,
  typeIntoTransformBody,
  checkTransformRequestUrlError,
  checkTransformRequestBodyError,
  checkTransformRequestUrlPreview,
  clearPayloadTransformBody,
  clearRequestUrl,
  toggleContextArea,
  typeIntoContextAreaEnvVars,
  getActionTransfromV1RequestBody,
} from '../../helpers/webhookTransformHelpers';

const ACTION_REQUEST_BODY_TRANSFORM_TEXTAREA = 4;

const statements = {
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
  createTransformEnvHandler: '{{MY_WEBHOOK}}',
  createTransformHandler: 'https://hasura-actions-demo.glitch.me',
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
  cy.getBySel('action-create-handler-input').type('{selectall}{backspace}', {
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
  cy.getBySel('action-create-handler-input').type(content, {
    force: true,
    parseSpecialCharSequences: false,
  });
};

const clickOnCreateAction = () => {
  cy.getBySel('create-action-btn').scrollIntoView();
  // hard await before accessing the element
  cy.wait(AWAIT_SHORT);

  cy.getBySel('create-action-btn').click({ force: true });
  cy.wait(AWAIT_SHORT);
  cy.get('.notification', { timeout: AWAIT_LONG })
    .should('be.visible')
    .and('contain', 'Created action successfully');
};

export const routeToGraphiql = () => {
  cy.visit('/api/api-explorer');
  cy.url({ timeout: AWAIT_LONG }).should('eq', `${baseUrl}/api/api-explorer`);
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

const deleteAction = (promptValue: string) => {
  setPromptValue(promptValue);
  cy.getBySel('delete-action').click();
  cy.window().its('prompt').should('be.called');
};

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
  cy.getBySel('data-create-actions').click();
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

  cy.getBySel('save-modify-action-changes').click();

  // permissions part
  cy.getBySel('actions-permissions').click();

  cy.getBySel('role-textbox').type('MANAGER');

  cy.getBySel('MANAGER-Permission').click();
  cy.getBySel('save-permissions-for-action').click();

  cy.getBySel('actions-modify').click();
};

export const deleteQueryAction = () => deleteAction('addNumbers');

export const createActionTransform = () => {
  // Click on create
  cy.getBySel('actions-sidebar-add-table').click();
  // Clear default text on
  clearActionDef();
  // type statement
  typeIntoActionDef(statements.createActionTransform);
  // clear defaults on action types
  clearActionTypes();
  // type the action type text
  typeIntoActionTypes(statements.createTransformCustomType);
  cy.getBySel('action-timeout-seconds').clear().type(getTimeoutSeconds());
  // open request transform section
  toggleRequestTransformSection();
  cy.wait(AWAIT_SHORT);
  cy.getBySel('transform-POST').click();

  // give correct body without webhook handler
  clearHandler();
  typeIntoRequestUrl('users');
  cy.wait(AWAIT_SHORT);
  // check for error
  checkTransformRequestUrlError(
    true,
    'Please configure your webhook handler to generate request url transform'
  );

  // give correct body with env var
  clearHandler();
  typeIntoHandler(statements.createTransformEnvHandler);
  // give body without specifying env var
  clearRequestUrl();
  typeIntoRequestUrl('/users');
  cy.wait(AWAIT_SHORT);
  // check for error
  checkTransformRequestUrlError(true);

  // add env var in context area
  toggleContextArea();
  typeIntoContextAreaEnvVars([
    { key: 'MY_WEBHOOK', value: 'https://handler.com' },
  ]);
  // check there is no error and preview works fine
  checkTransformRequestUrlError(false);
  checkTransformRequestUrlPreview('https://handler.com/users');

  // clear handler
  clearHandler();
  // type into handler
  typeIntoHandler(statements.createTransformHandler);

  // give incorrect body
  clearRequestUrl();
  typeIntoRequestUrl('{{$url}}/users');
  cy.wait(AWAIT_SHORT);
  // check for error
  checkTransformRequestUrlError(true);

  // give correct body
  clearRequestUrl();
  typeIntoRequestUrl('/{{$body.action.name}}');
  cy.wait(AWAIT_SHORT);
  typeIntoRequestQueryParams([
    { key: 'id', value: '5' },
    { key: 'name', value: '{{$body.action.name}}' },
  ]);
  cy.wait(AWAIT_SHORT);
  // check there is no error
  checkTransformRequestUrlError(false);
  // check the preview is correctly shown
  checkTransformRequestUrlPreview(
    'https://hasura-actions-demo.glitch.me/login?name=login&id=5'
  );

  // open payload transform section
  togglePayloadTransformSection();

  // give incorrect body
  clearPayloadTransformBody(ACTION_REQUEST_BODY_TRANSFORM_TEXTAREA);
  typeIntoTransformBody(
    statements.createTransformIncorrectPayloadBody,
    ACTION_REQUEST_BODY_TRANSFORM_TEXTAREA
  );
  cy.wait(AWAIT_SHORT);
  checkTransformRequestBodyError(true);

  // give correct body
  clearPayloadTransformBody(ACTION_REQUEST_BODY_TRANSFORM_TEXTAREA);
  typeIntoTransformBody(
    statements.createTransformPayloadBody,
    ACTION_REQUEST_BODY_TRANSFORM_TEXTAREA
  );
  cy.wait(AWAIT_SHORT);
  checkTransformRequestBodyError(false);

  // click to create action
  cy.intercept('*', req => {
    // send all other requests to the destination server
    req.reply();
  });
  clickOnCreateAction();
  cy.getBySel('action-timeout-seconds').should('have.value', '25');
};

export const deleteActionTransform = () => deleteAction('login');

const createV1ActionTransform = (actionName: string) => {
  cy.request(
    'POST',
    'http://localhost:8080/v1/metadata',
    getActionTransfromV1RequestBody(actionName)
  ).then(response => {
    expect(response.body).to.not.be.null;
    expect(response.body).to.be.a('array');
    expect(response.body[0]).to.have.property('message', 'success'); // true
  });
};

export const modifyV1ActionTransform = () => {
  // Creates an action with v1 transform
  createV1ActionTransform('login');

  cy.wait(AWAIT_SHORT);
  // modify and save the action, the action should be converted into v2
  cy.visit('/actions/manage/login/modify');
  cy.url({ timeout: AWAIT_LONG }).should(
    'eq',
    `${baseUrl}/actions/manage/login/modify`
  );
  cy.getBySel('transform-POST').click();
  cy.getBySel('transform-requestUrl')
    .clear()
    .type('/{{$body.action.name}}/actions', {
      parseSpecialCharSequences: false,
    });

  cy.getBySel('save-modify-action-changes').click();
  cy.get('.notification', { timeout: AWAIT_LONG })
    .should('be.visible')
    .and('contain', 'Action saved successfully');

  // delete the action
  deleteActionTransform();
};
