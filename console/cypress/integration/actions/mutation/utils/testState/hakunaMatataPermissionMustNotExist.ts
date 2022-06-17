import { readMetadata } from '../services/readMetadata';
import { deleteHakunaMatataPermission } from '../services/deleteHakunaMatataPermission';

/**
 * Ensure the Action does not have the Permission.
 *
 * ATTENTION: if you get the "setup function changed for session..." error, simply close the
 * Cypress-controlled browser and re-launch the test file.
 */
export function hakunaMatataPermissionMustNotExist(
  settingUpApplicationState = true
) {
  cy.dataSession({
    name: 'hakunaMatataPermissionMustNotExist',

    // Without it, cy.dataSession run the setup function also the very first time, trying to
    // delete a Permission that does not exist
    init: () => true,

    // Check if the Permission exists
    validate: () => {
      Cypress.log({ message: '**--- Action check: start**' });

      return readMetadata().then(response => {
        const loginAction = response.body.actions?.find(
          // TODO: properly type it
          action => action.name === 'login'
        );

        if (!loginAction || !loginAction.permissions) return true;

        const permission = loginAction.permissions.find(
          permission => permission.role === 'hakuna_matata'
        );

        // Returns true if the permission does not exist
        return !permission;
      });
    },

    preSetup: () =>
      Cypress.log({ message: '**--- The permission must be deleted**' }),

    // Delete the Permission
    setup: () => {
      deleteHakunaMatataPermission();

      if (settingUpApplicationState) {
        // Ensure the UI read the latest data if it were previously loaded
        cy.reload();
      }
    },
  });
}
