import { readMetadata } from '../services/readMetadata';
import { deleteAddNumbersAction } from '../services/deleteAddNumbersAction';

/**
 * Ensure the Action does not exist.
 */
export function addNumbersActionMustNotExist() {
  Cypress.log({ message: '**--- Action check: start**' });

  readMetadata().then(response => {
    const actionExists = !!response.body.actions?.find(
      // TODO: properly type it
      action => action.name === 'addNumbers'
    );

    if (actionExists) {
      Cypress.log({ message: '**--- The Action must be deleted**' }),
        deleteAddNumbersAction();
    }
  });
}
