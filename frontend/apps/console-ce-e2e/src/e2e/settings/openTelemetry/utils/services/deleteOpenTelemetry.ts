/**
 * Delete OpenTelemetry straight on the server.
 */
import { hgeUrl } from '../../../../../support/endpoints';
export function deleteOpenTelemetry() {
  Cypress.log({ message: '**--- OpenTelemetry delete: start**' });

  return cy
    .request('POST', hgeUrl('/v1/metadata'), {
      type: 'set_opentelemetry_config',
      args: {},
    })
    .then(() => Cypress.log({ message: '**--- OpenTelemetry delete: end**' }));
}
