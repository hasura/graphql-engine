import { testMode } from '../../../helpers/common';

import { logMetadataRequests } from './utils/requests/logMetadataRequests';
import { loginActionMustNotExist } from './utils/testState/loginActionMustNotExist';

// NOTE: This test suite does not include cases for relationships, headers and the codegen part

if (testMode !== 'cli') {
  describe('Mutation Actions', () => {
    before(() => {
      loginActionMustNotExist();
      logMetadataRequests();

      cy.visit('/actions/manage/actions');
    });

    after(() => {
      // Cleanup after the whole test file run

      // Endure the application is not there when manually deleting the created action to avoid any
      // potential client-side error that makes the test fail
      cy.visitEmptyPage();

      // Delete the created action, if any
      loginActionMustNotExist({ reloadPage: false });
    });

    it('When the users create, edit, and delete a Mutation Action, everything should work', () => {
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**--- Step 1: Mutation Action creation**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');

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
          }
          `,
        { force: true, delay: 0 }
      );

      cy.log('**--- Type in the Webhook Handler field**');
      cy.getBySel('action-create-handler-input')
        .clearConsoleTextarea()
        .type('https://hasura-actions-demo.glitch.me/login', {
          delay: 0,
          parseSpecialCharSequences: false,
        });

      // Please note: we should wait for the outgoing request but at the moment of writing, I'm not
      // sure the Console locally works as the one in CI. The request was `test_webhook_transform`

      // --------------------
      // Pleas note that the custom timeout is not checked explicitly checked, but asserting on
      // the payload (see the next cy.intercept) means asserting on it too
      cy.log('**--- Type in the Custom Timeout field**');
      cy.getBySel('action-timeout-seconds').clear().type('25');

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
      cy.log('**--- Step 2: Permission add**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');

      cy.log('**--- Go the the action page**');
      cy.getBySel('actions-table-links').within(() => {
        cy.getBySel('login').click();
      });

      // --------------------
      cy.log('**--- Click the Permissions tab**');
      cy.getBySel('actions-permissions').click();

      // --------------------
      cy.log('**--- Enter a new role**');
      cy.getBySel('role-textbox').type('hakuna_matata');
      cy.getBySel('hakuna_matata-Permission').click();

      cy.getBySel('save-permissions-for-action').click();

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
        .and('contain', 'Permission saved successfully');

      // TODO: check if it exists in the database? Other tests do that

      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**--- Step 3: Action delete**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');

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
