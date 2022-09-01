/**
 * Delete the Action straight from the server.
 */
export function deleteV1LoginAction() {
  Cypress.log({ message: '**--- Action delete: start**' });

  return cy
    .request('POST', 'http://localhost:8080/v1/metadata', {
      type: 'drop_action',
      args: { name: 'v1Login' },
    })
    .then(() => Cypress.log({ message: '**--- Action delete: end**' }));
}
