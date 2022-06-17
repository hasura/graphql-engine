import { readMetadata } from '../services/readMetadata';
import { deleteLoginAction } from '../services/deleteLoginAction';

interface Options {
  reloadPage: boolean;
}

const defaultOptions: Options = {
  reloadPage: false,
};

/**
 * Ensure the Action does not exist.
 *
 * ATTENTION: if you get the "setup function changed for session..." error, simply close the
 * Cypress-controlled browser and re-launch the test file.
 */
export function loginActionMustNotExist(options = defaultOptions) {
  cy.dataSession({
    name: 'loginActionMustNotExist',

    // Without it, cy.dataSession run the setup function also the very first time, trying to
    // delete an Action that does not exist.
    init: () => true,

    // Check if the Action exists
    validate: () => {
      Cypress.log({ message: '**--- Action check: start**' });

      return readMetadata().then(response => {
        const action = response.body.actions?.find(
          // TODO: properly type it
          action => action.name === 'login'
        );

        // Return true if the action does no exist
        return !action;
      });
    },

    preSetup: () =>
      Cypress.log({ message: '**--- The Action must be deleted**' }),

    // Delete the existing action
    setup: () => {
      deleteLoginAction();

      if (options.reloadPage) {
        // Ensure the UI read the latest data if it were previously loaded
        cy.reload();
      }
    },
  });
}
