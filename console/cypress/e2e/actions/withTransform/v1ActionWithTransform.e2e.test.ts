import { testMode } from '../../../helpers/common';

import { logMetadataRequests } from './utils/requests/logMetadataRequests';
import { v1LoginActionMustExist } from './utils/testState/v1LoginActionMustExist';
import { v1LoginActionMustNotExist } from './utils/testState/v1LoginActionMustNotExist';

if (testMode !== 'cli') {
  describe('V1 Actions with Transform', () => {
    before(() => {
      v1LoginActionMustExist();
      logMetadataRequests();

      cy.visit('/actions/manage/v1Login/modify');
    });

    after(() => {
      // Delete the created action, if any
      v1LoginActionMustNotExist();
    });

    it('When the users edit a V1 Action with Transform, everything should work', () => {
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**--- Step 3: Mutation Action delete**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');

      cy.get('[data-cy="Change Request Options"]').within(() => {
        // --------------------
        cy.log('**--- Choose POST**');
        cy.contains('POST').click();

        // --------------------
        cy.log('**--- Type in the Request URL Template field**');
        cy.get('[placeholder="URL Template (Optional)..."]').type(
          '/{{$body.action.name}}/actions',
          {
            delay: 0,
            parseSpecialCharSequences: false,
          }
        );
      });

      // --------------------
      cy.log('**--- Click on the Save button**');
      cy.getBySel('save-modify-action-changes').click();

      // --------------------
      cy.log('**--- Check if the success notification is visible**');
      cy.get(
        '.notification',
        // The custom timeout aims to replace the lack of waiting for the outgoing request
        { timeout: 10000 }
      )
        .should('be.visible')
        .and('contain', 'Action saved successfully');

      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**--- Step 2: Action delete**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');
      cy.log('**------------------------------**');

      // --------------------
      cy.log('**--- Go the the action page**');
      cy.getBySel('actions-table-links').within(() => {
        cy.getBySel('v1Login').click();
      });

      // --------------------
      cy.log('**--- Set the prompt value**');
      cy.window().then(win => cy.stub(win, 'prompt').returns('v1Login'));

      cy.log('**--- Click the Delete button**');
      cy.getBySel('delete-action').click();

      // --------------------
      cy.log('**--- Check the prompt has been called**');
      cy.window().its('prompt').should('be.called');

      // Due to the double server/cli mode behavior, we do not assert about the XHR request payload here

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
