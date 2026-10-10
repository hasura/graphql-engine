/**
 * Delete the Action straight from the server.
 */
import { hgeUrl } from '../../../../../support/endpoints';
export function deleteV1LoginAction() {
  Cypress.log({ message: '**--- Action delete: start**' });

  return cy
    .request('POST', hgeUrl('/v1/metadata'), {
      type: 'drop_action',
      args: { name: 'v1Login' },
    })
    .then(() => Cypress.log({ message: '**--- Action delete: end**' }));
}
