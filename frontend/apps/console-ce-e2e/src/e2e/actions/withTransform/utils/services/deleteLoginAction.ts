/**
 * Delete the Action straight from the server.
 */
import { hgeUrl } from '../../../../../support/endpoints';
export function deleteLoginAction() {
  Cypress.log({ message: '**--- Action delete: start**' });

  return cy
    .request('POST', hgeUrl('/v1/metadata'), {
      type: 'drop_action',
      args: { name: 'login' },
    })
    .then(() => Cypress.log({ message: '**--- Action delete: end**' }));
}
