/**
 * Delete the Permission straight from the server.
 */
export function deleteHakunaMatataPermission() {
  Cypress.log({ message: '**--- Permission delete: start**' });

  return cy
    .request('POST', 'http://localhost:8080/v1/metadata', {
      type: 'drop_action_permission',
      args: { action: 'login', role: 'hakuna_matata' },
    })
    .then(() => Cypress.log({ message: '**--- Permission delete: end**' }));
}
