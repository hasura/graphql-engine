import type { Metadata } from '@hasura/console-legacy-ce';
import { logMetadataRequests } from './utils/requests/logMetadataRequests';
import { readMetadata } from './utils/services/readMetadata';
import { loginActionMustNotExist } from './utils/testState/loginActionMustNotExist';
import { checkMetadataPayload } from '../../utils/checkMetadataPayload';

describe('Actions with Transform', () => {
  before(() => {
    loginActionMustNotExist();
    logMetadataRequests();

    cy.visit('/actions/manage/actions');
  });

  after(() => {
    // Delete the created action, if any
    loginActionMustNotExist();
  });

  it('When the users create, and delete a Action with Transform, everything should work', () => {
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 1: Action with Transform creation**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    // --------------------
    cy.log('**--- Click on the Create button of the Actions panel**');
    cy.get('[data-testid=data-create-actions]').click();

    // Assign an alias to the most unclear selectors for future references
    cy.get('textarea').eq(0).as('actionDefinitionTextarea');
    cy.get('textarea').eq(1).as('typeConfigurationTextarea');

    // --------------------
    cy.log('**--- Type in the Action Definition textarea**');
    cy.get('@actionDefinitionTextarea')
      .clearConsoleTextarea()
      .type(
        `type Mutation {
              login (username: String!, password: String!): LoginResponse
            }`,
        { force: true, delay: 0 }
      );

    // --------------------
    cy.log('**--- Type in the Type Configuration textarea**');
    cy.get('@typeConfigurationTextarea')
      .clearConsoleTextarea()
      .type(
        `type LoginResponse {
          accessToken: String!
        }`,
        { force: true, delay: 0 }
      );

    // --------------------
    cy.log('**--- Click the Add Request Options Transform button**');
    cy.contains('Add Request Options Transform').click();

    cy.log('**------------------------------**');
    cy.log('**--- Step 1.1: Add URL**');
    cy.log('**------------------------------**');

    cy.get('[data-cy="Change Request Options"]').within(() => {
      // --------------------
      cy.log('**--- Choose POST**');
      cy.contains('POST').click();

      // --------------------
      cy.log('**--- Type in the Request URL Template field**');
      cy.get('[placeholder="URL Template (Optional)..."]').type('/users');
    });

    cy.log('**------------------------------**');
    cy.log('**--- Step 1.2: Add Env Var**');
    cy.log('**------------------------------**');

    // --------------------
    cy.log('**--- Type in the Webhook Handler field**');
    cy.get('[data-test=action-create-handler-input]')
      .clearConsoleTextarea()
      .type('{{MY_WEBHOOK}}', {
        delay: 0,
        parseSpecialCharSequences: false,
      });

    // --------------------
    cy.log('**--- Click the Show Sample Context button**');
    cy.contains('Show Sample Context').click();

    // --------------------
    cy.log('**--- Type in the Env Variables Key field**');
    cy.get('[data-test=transform-env-vars-kv-key-0]').type('MY_WEBHOOK', {
      delay: 1,
    });
    cy.log('**--- Type in the Env Variables Value field**');
    cy.get('[data-test=transform-env-vars-kv-value-0]').type(
      'https://handler.com',
      {
        delay: 1,
      }
    );

    // --------------------
    cy.get('[data-cy="Change Request Options"]').within(() => {
      cy.log('**--- Check the Preview of the Request URL Template**');
      cy.get('[data-test=transform-requestUrl-preview]').should(
        'have.value',
        'https://handler.com/users'
      );
    });

    cy.log('**------------------------------**');
    cy.log('**--- Step 1.3: Add path**');
    cy.log('**------------------------------**');

    // --------------------
    cy.log('**--- Type in the Webhook Handler field**');
    cy.get('[data-test=action-create-handler-input]')
      .clearConsoleTextarea()
      .type('https://hasura-actions-demo.glitch.me', {
        delay: 0,
        parseSpecialCharSequences: false,
      });

    cy.log('**------------------------------**');
    cy.log('**--- Step 1.4: Query Params add**');
    cy.log('**------------------------------**');

    cy.get('[placeholder="URL Template (Optional)..."]')
      .clearConsoleTextarea()
      .type('/{{$body.action.name}}', { parseSpecialCharSequences: false });

    // --------------------
    cy.log('**--- Type in the first Query Params Key field**');
    cy.get('[data-test=transform-query-params-kv-key-0]').type('id');
    cy.log('**--- Type in the first Query Params Value field**');
    cy.get('[data-test=transform-query-params-kv-value-0]').type('5');

    // --------------------
    cy.log('**--- Type in the second Query Params Key field**');
    cy.get('[data-test=transform-query-params-kv-key-1]').type('name');
    cy.log('**--- Type in the second Query Params Value field**');
    cy.get('[data-test=transform-query-params-kv-value-1]').type(
      '{{$body.action.name}}',
      {
        parseSpecialCharSequences: false,
        delay: 0,
      }
    );

    // --------------------
    cy.get('[data-cy="Change Request Options"]').within(() => {
      cy.log('**--- Check the Preview of the Request URL Template**');

      cy.get('[name=request_url_preview]')
        .invoke('attr', 'value')
        .should('contains', 'name=login');
      cy.get('[name=request_url_preview]')
        .invoke('attr', 'value')
        .should('contains', 'id=5');
    });

    cy.log('**------------------------------**');
    cy.log('**--- Step 1.5: Add Payload Transform**');
    cy.log('**------------------------------**');

    // --------------------
    cy.log('**--- Click the Add Payload Transform button**');
    cy.contains('Add Payload Transform').click();

    // --------------------
    cy.get('[data-cy="Change Payload"]').within(() => {
      // Assign an alias to the most unclear selectors for future references
      cy.get('textarea').eq(1).as('payloadTransformRequestBody');

      cy.log('**--- Type in the Payload Transform Request Body textarea**');
      cy.get('@payloadTransformRequestBody')
        .wait(500)
        .clearConsoleTextarea()
        .wait(1000) // Work around the fact that this test fails in CI but not locally
        .clearConsoleTextarea()
        .wait(1000) // Work around the fact that this test fails in CI but not locally
        .type(
          `{
            "userInfo": {
              "name": {{$body.input.username}},
              "password": {{$body.input.password}},
              "type": {{$body.action.name}}
            `,
          // delay is set to 1 because setting it to 0 causes the test to fail because writes
          // something like
          // "name": {{$body.input.username}}name
          // in the textarea (the closing "name" is a mistake)
          { force: true, delay: 1, parseSpecialCharSequences: false }
        );
    });

    cy.log('**------------------------------**');
    cy.log('**--- Step 1.5: Add Response Transform**');
    cy.log('**------------------------------**');

    // --------------------
    cy.log('**--- Click the Add Response Transform button**');
    cy.contains('Add Response Transform').click({ force: true });

    // --------------------
    cy.get('[data-cy="Change Response"]').within(() => {
      // Assign an alias to the most unclear selectors for future references
      cy.get('textarea').eq(0).as('responseTransformResponseBody');

      cy.log('**--- Type in the Response Transform Response Body textarea**');
      cy.get('@responseTransformResponseBody')
        .wait(500)
        .clearConsoleTextarea()
        .wait(500)
        .clearConsoleTextarea()
        .wait(500)
        .type(
          `{
            "userInfo": {
              "name": {{$body.input.username}},
              "password": {{$body.input.password}},
              "type": {{$body.action.name}}
            `,
          // delay is set to 1 because setting it to 0 causes the test to fail because writes
          // something like
          // "name": {{$body.input.username}}name
          // in the textarea (the closing "name" is a mistake)
          { force: true, delay: 1, parseSpecialCharSequences: false }
        );
    });

    // --------------------
    cy.log('**--- Click the Create button**');
    // cy.wait(1000) because of debounce

    cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
      if (JSON.stringify(req.body).includes('create_action')) {
        req.alias = 'createAction';
      }
      req.continue();
    });

    cy.intercept('POST', 'http://localhost:9693/apis/migrate', req => {
      if (JSON.stringify(req.body).includes('create_action')) {
        req.alias = 'createAction';
      }
    });

    cy.wait(1000);

    cy.get('[data-test=create-action-btn]').click();

    cy.wait('@createAction').then(interception => {
      checkMetadataPayload(interception, { name: 'Action payload' });
    });

    // --------------------
    cy.log('**--- Check if the success notification is visible**');
    cy.expectSuccessNotificationWithTitle('Created action successfully');

    // -------------------------------------------------------------------------
    // see: https://github.com/hasura/graphql-engine-mono/issues/5433
    // The "Action change" part has been removed since it caused Cypress to crash
    // TODO: identify the crashing reason
    // -------------------------------------------------------------------------

    // cy.log('**------------------------------**');
    // cy.log('**------------------------------**');
    // cy.log('**------------------------------**');
    // cy.log('**--- Step 2: Action change**');
    // cy.log('**------------------------------**');
    // cy.log('**------------------------------**');
    // cy.log('**------------------------------**');

    readMetadata().then((md: { body: Metadata['metadata'] }) => {
      cy.wrap(
        (md.body.actions || []).find(action => action.name === 'login')
      ).toMatchSnapshot({ name: 'Action metadata' });
    });

    // // --------------------
    cy.log('**--- Wait all the requests to be settled**');

    // cy.get('[data-cy="Change Request Options"]').within(() => {
    //   // --------------------
    //   cy.log('**--- Choose GET**');
    //   cy.contains('GET').click();

    //   // --------------------
    //   cy.log('**--- Type in the Request URL Template field**');

    //   cy.get('[placeholder="URL Template (Optional)..."]')
    //     .clearConsoleTextarea()
    //     .type('/{{$body.action.name}}/actions', {
    //       delay: 0,
    //       parseSpecialCharSequences: false,
    //     });

    //   // --------------------
    //   cy.log('**--- Click on the first Remove Query Param button**');
    //   cy.get('[data-test=transform-query-params-kv-remove-button-0]').click();
    // });

    // // --------------------
    // cy.log('**--- Click the Remove Payload Transform button**');
    // cy.contains('Remove Payload Transform').click();

    // // --------------------
    // cy.log('**--- Click on the Save button**');
    // cy.get('[data-test=save-modify-action-changes]').click();

    // // --------------------
    // cy.log('**--- Check if the success notification is visible**');
    // cy.expectSuccessNotificationWithTitle('Action saved successfully');

    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**--- Step 3: Action delete**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');
    cy.log('**------------------------------**');

    // --------------------
    cy.log('**--- Go the the action page**');
    cy.get('[data-test=actions-table-links]').within(() => {
      cy.get('[data-test=login]').click();
    });

    // --------------------
    cy.log('**--- Set the prompt value**');
    cy.window().then(win => cy.stub(win, 'prompt').returns('login'));

    cy.log('**--- Click the Delete button**');
    cy.get('[data-test=delete-action]').click();

    // --------------------
    cy.log('**--- Check the prompt has been called**');
    cy.window().its('prompt').should('be.called');

    // --------------------
    cy.log('**--- Check if the success notification is visible**');
    cy.expectSuccessNotificationWithTitle('Action deleted successfully');
  });
});
