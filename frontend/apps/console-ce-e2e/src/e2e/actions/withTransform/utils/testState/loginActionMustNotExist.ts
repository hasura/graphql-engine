import { readMetadata } from '../services/readMetadata';
import { deleteLoginAction } from '../services/deleteLoginAction';

/**
 * Ensure the Action does not exist.
 */
export function loginActionMustNotExist() {
  Cypress.log({ message: '**--- Action check: start**' });

  readMetadata().then(response => {
    const actionExists = !!response.body.actions?.find(
      // TODO: properly type it
      action => action.name === 'login'
    );

    if (actionExists) {
      Cypress.log({ message: '**--- The Action must be deleted**' });
      deleteLoginAction();
    }
  });
}
