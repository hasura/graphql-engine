/**
 * Delete OpenTelemetry straight on the server.
 */
export function deleteOpenTelemetry() {
  Cypress.log({ message: '**--- OpenTelemetry delete: start**' });

  return cy
    .request('POST', 'http://localhost:8080/v1/metadata', {
      type: 'set_opentelemetry_config',
      args: {},
    })
    .then(() => Cypress.log({ message: '**--- OpenTelemetry delete: end**' }));
}
