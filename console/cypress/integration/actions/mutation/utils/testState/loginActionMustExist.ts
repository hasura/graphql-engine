import { readMetadata } from '../services/readMetadata';
import { createLoginAction } from '../services/createLoginAction';

/**
 * Ensure the Action exists.
 *
 * ATTENTION: if you get the "setup function changed for session..." error, simply close the
 * Cypress-controlled browser and re-launch the test file.
 */
export function loginActionMustExist(settingUpApplicationState = true) {
  cy.dataSession({
    name: 'loginActionMustExist',

    // Without it, cy.dataSession run the setup function also the very first time, trying to
    // create an Action that already exists
    init: () => true,

    // Check if the Action exists
    validate: () => {
      Cypress.log({ message: '**--- Action check: start**' });

      return readMetadata().then(response => {
        const action = response.body.actions?.find(
          // TODO: properly type it
          action => action.name === 'login'
        );

        // Return true if the action exists
        return !!action;
      });
    },

    preSetup: () =>
      Cypress.log({ message: '**--- The Action must be created**' }),

    // Create the action
    setup: () => {
      createLoginAction();

      if (settingUpApplicationState) {
        // Ensure the UI read the latest data if it were previously loaded
        cy.reload();
      }
    },
  });
}
