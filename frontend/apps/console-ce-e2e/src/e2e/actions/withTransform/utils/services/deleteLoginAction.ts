/**
 * Delete the Action straight from the server.
 */
export function deleteLoginAction() {
  Cypress.log({ message: '**--- Action delete: start**' });

  return cy
    .request('POST', 'http://localhost:8080/v1/metadata', {
      type: 'drop_action',
      args: { name: 'login' },
    })
    .then(() => Cypress.log({ message: '**--- Action delete: end**' }));
}
