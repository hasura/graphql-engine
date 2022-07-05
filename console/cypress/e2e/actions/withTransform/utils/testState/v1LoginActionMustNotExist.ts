import { readMetadata } from '../services/readMetadata';
import { deleteV1LoginAction } from '../services/deleteV1LoginAction';

/**
 * Ensure the Action does not exist.
 */
export function v1LoginActionMustNotExist() {
  Cypress.log({ message: '**--- Action check: start**' });

  readMetadata().then(response => {
    const actionExists = !!response.body.actions?.find(
      // TODO: properly type it
      action => action.name === 'v1Login'
    );

    if (actionExists) {
      Cypress.log({ message: '**--- The Action must be deleted**' }),
        deleteV1LoginAction();
    }
  });
}
