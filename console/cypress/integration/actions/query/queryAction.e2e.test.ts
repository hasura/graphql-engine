import { testMode } from '../../../helpers/common';

import { logMetadataRequests } from './utils/requests/logMetadataRequests';
import { addNumbersActionMustNotExist } from './utils/testState/addNumbersActionMustNotExist';

// NOTE: This test suite does not include cases for relationships, headers and the codegen part

if (testMode !== 'cli') {
  describe('Query Actions', () => {
    before(() => {
      addNumbersActionMustNotExist();
      logMetadataRequests();

      cy.visit('/actions/manage/actions');
    });

    after(() => {
      // Cleanup after the whole test file run

      // Endure the application is not there when manually deleting the created action to avoid any
      // potential client-side error that makes the test fail
      cy.visitEmptyPage();

      // Delete the created action, if any
      addNumbersActionMustNotExist();
    });

    it('When the users create, edit, and delete a Query Action, everything should work', () => {
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**--- Step 1: Query Action creation**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');

      // --------------------
      cy.log('**--- Click on the Create button of the Actions panel**');
      cy.getBySel('data-create-actions').click();

      // --------------------
      // Assign an alias to the most unclear selectors for future references
      cy.get('textarea').eq(0).as('actionDefinitionTextarea');
      cy.get('textarea').eq(1).as('typeConfigurationTextarea');

      // --------------------
      cy.log('**--- Type in the Action Definition textarea**');
      cy.get('@actionDefinitionTextarea').clearConsoleTextarea().type(
        `type Query {
          addNumbers (numbers: [Int]): AddResult
        }`,
        { force: true, delay: 0 }
      );

      // --------------------
      cy.log('**--- Type in the Type Configuration textarea**');
      cy.get('@typeConfigurationTextarea').clearConsoleTextarea().type(
        `type AddResult {
          sum: Int
        }`,
        { force: true, delay: 0 }
      );

      // --------------------
      cy.log('**--- Type in the Webhook Handler field**');
      cy.getBySel('action-create-handler-input')
        .clearConsoleTextarea()
        .type('https://hasura-actions-demo.glitch.me/addNumbers', {
          delay: 0,
          parseSpecialCharSequences: false,
        });

      // Please note: we should wait for the outgoing request but at the moment of writing, I'm not
      // sure the Console locally works as the one in CI. The request was `test_webhook_transform`

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
      cy.log('**--- Step 2: Permission add and Handler change**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');

      // --------------------

      cy.log('**--- Go the the action page**');
      cy.getBySel('actions-table-links').within(() => {
        cy.getBySel('addNumbers').click();
      });

      // --------------------
      cy.log('**--- Type in the Webhook Handler field**');
      cy.getBySel('action-create-handler-input')
        .clearConsoleTextarea()
        .type('http://host.docker.internal:3000', {
          delay: 0,
          // parseSpecialCharSequences: false,
        });

      // --------------------
      cy.log('**--- Click on the Save button**');
      cy.getBySel('save-modify-action-changes').click();

      // --------------------
      cy.log('**--- Click the Permissions tab**');
      cy.getBySel('actions-permissions').click();

      // --------------------
      cy.log('**--- Enter a new role**');
      cy.getBySel('role-textbox').type('manager');
      cy.getBySel('manager-Permission').click();

      // --------------------
      cy.log('**--- Click Save Permissions**');
      cy.getBySel('save-permissions-for-action').click();

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
      cy.log('**--- Step 3: Query Action delete**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');

      // --------------------
      cy.log('**--- Go the the action page**');
      cy.getBySel('actions-table-links').within(() => {
        cy.getBySel('addNumbers').click();
      });

      // --------------------
      cy.log('**--- Set the prompt value**');
      cy.window().then(win => cy.stub(win, 'prompt').returns('addNumbers'));

      cy.log('**--- Click the Delete button**');
      cy.getBySel('delete-action').click();

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
