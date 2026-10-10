/**
 * Freeze and check the request and response payloads.
 *
 * TODO: properly type the interception.
 */

import { Interception } from 'cypress/types/net-stubbing';

type Options = { name?: string };

export function checkMetadataPayload(
  interception: Interception,
  options: Options,
) {
  let bodyToSnapshot: unknown;

  // console mode: server
  if (interception.request.url.includes('v1/metadata')) {
    const { resource_version, ...other } = interception.request.body;
    // `resource_version` is an OPTIONAL optimistic-concurrency token. The console
    // only sends it for metadata writes that opt into concurrency checks; bulk
    // action writes (set_custom_types + create_action) omit it. Assert its type
    // only when it is actually present, so the check stays meaningful without
    // failing on ops that legitimately don't send it.
    if (resource_version !== undefined) {
      expect(resource_version).to.be.a('number');
    }
    bodyToSnapshot = other.type === 'bulk' ? other.args : other;

    // console mode: cli
  } else {
    bodyToSnapshot = interception.request.body.up;
  }
  cy.wrap({
    bodyToSnapshot,
  }).toMatchSnapshot({ name: options.name });
}
