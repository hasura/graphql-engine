/**
 * Read the Metadata straight from the server.
 */
export function readMetadata() {
  Cypress.log({ message: '**--- Metadata read: start**' });

  return cy
    .request('POST', 'http://localhost:8080/v1/metadata', {
      args: {},
      type: 'export_metadata',
    })
    .then(() => {
      Cypress.log({ message: '**--- Metadata read: end**' });
    });
}
