/**
 * Read the Metadata straight from the server.
 */
import { hgeUrl } from '../../../../../support/endpoints';
export function readMetadata() {
  Cypress.log({ message: '**--- Metadata read: start**' });

  return cy
    .request('POST', hgeUrl('/v1/metadata'), {
      args: {},
      type: 'export_metadata',
    })
    .then(() => {
      Cypress.log({ message: '**--- Metadata read: end**' });
    });
}
