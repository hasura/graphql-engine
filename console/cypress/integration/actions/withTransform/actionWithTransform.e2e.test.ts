import { testMode } from '../../../helpers/common';

import { logMetadataRequests } from './utils/requests/logMetadataRequests';
import { loginActionMustNotExist } from './utils/testState/loginActionMustNotExist';
import { waitForPostCreationRequests } from './utils/requests/waitForPostCreationRequests';

if (testMode !== 'cli') {
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

    // TODO: This test is slow because it tests not only the happy path but also the error cases of
    // the Request Options Transform. Checking the error paths through an E2E test is a bad practice.

    it('When the users create, edit, and delete a Action with Transform, everything should work', () => {
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**--- Step 1: Action with Transform creation**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');

      // --------------------
      cy.log('**--- Click on the Create button of the Actions panel**');
      cy.getBySel('data-create-actions').click();

      // Assign an alias to the most unclear selectors for future references
      cy.get('textarea').eq(0).as('actionDefinitionTextarea');
      cy.get('textarea').eq(1).as('typeConfigurationTextarea');

      // --------------------
      cy.log('**--- Type in the Action Definition textarea**');
      cy.get('@actionDefinitionTextarea').clearConsoleTextarea().type(
        `type Mutation {
              login (username: String!, password: String!): LoginResponse
            }`,
        { force: true, delay: 0 }
      );

      // --------------------
      cy.log('**--- Type in the Type Configuration textarea**');
      cy.get('@typeConfigurationTextarea').clearConsoleTextarea().type(
        `type LoginResponse {
          accessToken: String!
        }`,
        { force: true, delay: 0 }
      );

      // --------------------
      // Pleas note that the custom timeout is not checked explicitly checked, but asserting on
      // the payload (see the next cy.intercept) means asserting on it too
      cy.log('**--- Type in the Custom Timeout field**');
      cy.getBySel('action-timeout-seconds').clearConsoleTextarea().type('25');

      // --------------------
      cy.log('**--- Click the Add Request Options Transform button**');
      cy.contains('Add Request Options Transform').click();

      cy.log('**------------------------------**');
      cy.log('**--- Step 1.1: Invalid URL error**');
      cy.log('**------------------------------**');

      cy.get('[data-cy="Change Request Options"]').within(() => {
        // --------------------
        cy.log('**--- Choose POST**');
        cy.contains('POST').click();

        // --------------------
        cy.log('**--- Type in the Request URL Template field**');
        cy.get('[placeholder="URL Template (Optional)..."]').type('users');

        // Please note: we should wait for the outgoing request but at the moment of writing, I'm not
        // sure the Console locally works as the one in CI.

        // --------------------
        cy.log('**--- Look for the "Invalid URL" error**');
        cy.contains(
          'TransformationError: Invalid URL: http://host.docker.internal:3000users',
          // Pleas note that the custom timeout is not checked explicitly checked, but asserting on
          // the payload (see the next cy.intercept) means asserting on it too
          { timeout: 10000 }
        ).should('be.visible');
      });

      cy.log('**------------------------------**');
      cy.log('**--- Step 1.2: Missing Env Var error**');
      cy.log('**------------------------------**');

      // --------------------
      cy.log('**--- Type in the Webhook Handler field**');
      cy.getBySel('action-create-handler-input')
        .clearConsoleTextarea()
        .type('{{MY_WEBHOOK}}', {
          delay: 0,
          parseSpecialCharSequences: false,
        });

      cy.get('[data-cy="Change Request Options"]').within(() => {
        // --------------------
        cy.log('**--- Type in the Request URL Template field**');
        cy.get('[placeholder="URL Template (Optional)..."]')
          .clearConsoleTextarea()
          .type('/users');

        // Please note: we should wait for the outgoing request but at the moment of writing, I'm not
        // sure the Console locally works as the one in CI.

        // --------------------
        cy.log('**--- Look for "Missing Env Var" error**');
        cy.getBySel('transform-requestUrl-error')
          .as('missingWebhookError')
          .should('be.visible')
          .and('contain', 'not-found: Missing Env Var: MY_WEBHOOK');
      });

      cy.log('**------------------------------**');
      cy.log('**--- Step 1.2: Env Var add**');
      cy.log('**------------------------------**');

      // --------------------
      cy.log('**--- Click the Show Sample Context button**');
      cy.contains('Show Sample Context').click();

      // --------------------
      cy.log('**--- Type in the Env Variables Key field**');
      cy.getBySel('transform-env-vars-kv-key-0').type('MY_WEBHOOK', {
        delay: 1,
      });
      cy.log('**--- Type in the Env Variables Value field**');
      cy.getBySel('transform-env-vars-kv-value-0').type('https://handler.com', {
        delay: 1,
      });

      // Please note: we should wait for the outgoing request but at the moment of writing, I'm not
      // sure the Console locally works as the one in CI.

      // --------------------
      cy.log('**--- Check the error disappeared**');
      cy.get(
        '@missingWebhookError',
        // Pleas note that the custom timeout is not checked explicitly checked, but asserting on
        // the payload (see the next cy.intercept) means asserting on it too
        { timeout: 10000 }
      ).should('not.exist');

      // --------------------
      cy.get('[data-cy="Change Request Options"]').within(() => {
        cy.log('**--- Check the Preview of the Request URL Template**');
        cy.getBySel('transform-requestUrl-preview').should(
          'have.value',
          'https://handler.com/users'
        );
      });

      cy.log('**------------------------------**');
      cy.log('**--- Step 1.3: Invalid Path error**');
      cy.log('**------------------------------**');

      // --------------------
      cy.log('**--- Type in the Webhook Handler field**');
      cy.getBySel('action-create-handler-input')
        .clearConsoleTextarea()
        .type('https://hasura-actions-demo.glitch.me', {
          delay: 0,
          parseSpecialCharSequences: false,
        });

      cy.get('[data-cy="Change Request Options"]').within(() => {
        // --------------------
        cy.log('**--- Type in the Request URL Template field**');
        cy.get('[placeholder="URL Template (Optional)..."]')
          .clearConsoleTextarea()
          .type('{{$url}}/users', { parseSpecialCharSequences: false });
      });

      // Please note: we should wait for the outgoing request but at the moment of writing, I'm not
      // sure the Console locally works as the one in CI.

      // --------------------
      cy.log('**--- Look for "Invalid Path" error**');

      cy.getBySel('transform-requestUrl-error')
        .as('invalidPathError')
        .should('be.visible')
        .and('contain', 'Invalid Path: "$url"');

      cy.log('**------------------------------**');
      cy.log('**--- Step 1.4: Query Params add**');
      cy.log('**------------------------------**');

      cy.get('[placeholder="URL Template (Optional)..."]')
        .clearConsoleTextarea()
        .type('/{{$body.action.name}}', { parseSpecialCharSequences: false });

      // --------------------
      cy.log('**--- Type in the first Query Params Key field**');
      cy.getBySel('transform-query-params-kv-key-0').type('id');
      cy.log('**--- Type in the first Query Params Value field**');
      cy.getBySel('transform-query-params-kv-value-0').type('5');

      // --------------------
      cy.log('**--- Type in the second Query Params Key field**');
      cy.getBySel('transform-query-params-kv-key-1').type('name');
      cy.log('**--- Type in the second Query Params Value field**');
      cy.getBySel('transform-query-params-kv-value-1').type(
        '{{$body.action.name}}',
        {
          parseSpecialCharSequences: false,
          delay: 0,
        }
      );

      // Please note: we should wait for the outgoing request but at the moment of writing, I'm not
      // sure the Console locally works as the one in CI.

      // --------------------
      cy.log('**--- Check the error disappeared**');
      cy.get(
        '@invalidPathError',
        // Pleas note that the custom timeout is not checked explicitly checked, but asserting on
        // the payload (see the next cy.intercept) means asserting on it too
        { timeout: 10000 }
      ).should('not.exist');

      // --------------------
      cy.get('[data-cy="Change Request Options"]').within(() => {
        cy.log('**--- Check the Preview of the Request URL Template**');
        cy.getBySel('transform-requestUrl-preview').should(
          'have.value',
          'https://hasura-actions-demo.glitch.me/login?name=login&id=5'
        );
      });

      cy.log('**------------------------------**');
      cy.log('**--- Step 1.5: Invalid Path error**');
      cy.log('**------------------------------**');

      // --------------------
      cy.log('**--- Click the Add Payload Transform button**');
      cy.contains('Add Payload Transform').click();

      // --------------------
      cy.get('[data-cy="Change Payload"]').within(() => {
        // Assign an alias to the most unclear selectors for future references
        cy.get('textarea').eq(1).as('payloadTransformRequestBody');

        // --------------------
        cy.log('**--- Type in the Payload Transform Request Body textarea**');
        cy.get('@payloadTransformRequestBody')
          .clearConsoleTextarea()
          // this second attempt tries to workaround a problem between Cypress and our text area.
          // In case the problem arises with other tests, it would be better off moving the double
          // clear to the clearConsoleTextarea command itself
          .clearConsoleTextarea()
          .type(
            `{
            "userInfo": {
              "name": {{$input.username}}
          `,
            { force: true, delay: 1, parseSpecialCharSequences: false }
          );

        // Please note: we should wait for the outgoing request but at the moment of writing, I'm not
        // sure the Console locally works as the one in CI.

        // --------------------
        cy.log('**--- Look for the "Invalid path" error**');
        cy.contains(
          `Invalid Path: "$input.username"`,
          // Pleas note that the custom timeout is not checked explicitly checked, but asserting on
          // the payload (see the next cy.intercept) means asserting on it too
          { timeout: 10000 }
        )
          .as('invalidPathError')
          .should('be.visible');

        cy.log('**------------------------------**');
        cy.log('**--- Step 1.6: Request Body add:**');
        cy.log('**------------------------------**');

        // --------------------
        cy.log('**--- Type in the Payload Transform Request Body textarea**');
        cy.get('@payloadTransformRequestBody').clearConsoleTextarea().type(
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

        // Please note: we should wait for the outgoing request but at the moment of writing, I'm not
        // sure the Console locally works as the one in CI.

        // --------------------
        cy.log('**--- Check the error disappeared**');
        cy.get(
          '@invalidPathError',
          // Pleas note that the custom timeout is not checked explicitly checked, but asserting on
          // the payload (see the next cy.intercept) means asserting on it too
          { timeout: 10000 }
        ).should('not.exist');
      });

      // --------------------
      cy.log('**--- Click the Create button**');
      cy.getBySel('create-action-btn').click();

      // Please note: we should wait for the outgoing request but at the moment of writing, I'm not
      // sure the Console locally works as the one in CI. The request was `create_action`

      // --------------------
      cy.log('**--- Check if the success notification is visible**');
      cy.get(
        '.notification',
        // The custom timeout aims to replace the lack of waiting for the outgoing request
        { timeout: 10000 }
      )
        .should('be.visible')
        .and('contain', 'Created action successfully');

      // TODO: check if it exists in the database? Other tests do that

      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**--- Step 2: Action change**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');

      // --------------------
      cy.log('**--- Wait all the requests to be settled**');
      waitForPostCreationRequests();

      cy.get('[data-cy="Change Request Options"]').within(() => {
        // --------------------
        cy.log('**--- Choose GET**');
        cy.contains('GET').click();

        // --------------------
        cy.log('**--- Type in the Request URL Template field**');

        cy.get('[placeholder="URL Template (Optional)..."]')
          .clearConsoleTextarea()
          .type('/{{$body.action.name}}/actions', {
            delay: 0,
            parseSpecialCharSequences: false,
          });

        // --------------------
        cy.log('**--- Click on the first Remove Query Param button**');
        cy.getBySel('transform-query-params-kv-remove-button-0').click();
      });

      // --------------------
      cy.log('**--- Click the Remove Payload Transform button**');
      cy.contains('Remove Payload Transform').click();

      // --------------------
      cy.log('**--- Click on the Save button**');
      cy.getBySel('save-modify-action-changes').click();

      // Please note: we should wait for the outgoing request but at the moment of writing, I'm not
      // sure the Console locally works as the one in CI. The request was `create_action_permission`

      // --------------------
      cy.log('**--- Check if the success notification is visible**');
      cy.get(
        '.notification',
        // The custom timeout aims to replace the lack of waiting for the outgoing request
        { timeout: 10000 }
      )
        .should('be.visible')
        .and('contain', 'Action saved successfully');

      // TODO: check if it exists in the database? Other tests do that

      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**--- Step 3: Action delete**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');

      // --------------------
      cy.log('**--- Reload the page**');
      // The purpose is to avoid the following notification when the Delete button will be clicked
      // -------------
      // Metadata is Out-of-Date
      // The operation failed as the metadata on the server is newer than what is currently loaded on the console. The metadata has to be re-fetched to continue editing it.
      // Do you want fetch the latest metadata?
      // Fetch metadata (button)
      // -------------
      cy.reload();

      // --------------------
      cy.log('**--- Go the the action page**');
      cy.getBySel('actions-table-links').within(() => {
        cy.getBySel('login').click();
      });

      // --------------------
      cy.log('**--- Set the prompt value**');
      cy.window().then(win => cy.stub(win, 'prompt').returns('login'));

      cy.log('**--- Click the Delete button**');
      cy.getBySel('delete-action').click();

      // --------------------
      cy.log('**--- Check the prompt has been called**');
      cy.window().its('prompt').should('be.called');

      // Please note: we should wait for the outgoing request but at the moment of writing, I'm not
      // sure the Console locally works as the one in CI. The request was `drop_action`

      // --------------------
      cy.log('**--- Check if the success notification is visible**');
      cy.get(
        '.notification',
        // The custom timeout aims to replace the lack of waiting for the outgoing request
        { timeout: 10000 }
      )
        .should('be.visible')
        .and('contain', 'Action deleted successfully');

      // TODO: check if it does not exist in the database? Other tests do that
    });
  });
}
