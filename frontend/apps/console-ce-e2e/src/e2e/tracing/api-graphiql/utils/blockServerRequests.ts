/**
 * Ensure no requests hit the server, it's mostly a safe protection for the developers running
 * server-free tests that could miss some requests are actually hitting the real server, then
 * resulting in test flakiness when because of partial server stubbing.
 */
import { hgeUrl } from '../../../../support/endpoints';
export function blockServerRequests() {
  cy.log('**--- Prevent any requests to hit the real server**');
  cy.intercept(hgeUrl('/**'), { forceNetworkError: true });
}
