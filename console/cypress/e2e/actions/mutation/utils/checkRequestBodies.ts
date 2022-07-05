/**
 * Freeze and check the request and response payloads.
 *
 * TODO: properly type the interception.
 */
export function checkRequestBodies(
  interception: any,
  options = { checkResourceVersion: true }
) {
  const { checkResourceVersion } = options;

  const {
    // The resource_version must be removed before snapshotting the request payload
    // because it is always different. It's going to be asserted in isolation
    resource_version,
    ...requestBody
  } = interception.request.body;

  cy.log('**--- Assert about the request**');

  if (checkResourceVersion) {
    expect(resource_version, 'resource_version').to.be.a('number');
  }

  cy.wrap({
    requestBody,
    statusCode: interception.response?.statusCode,
    responseBody: interception.response?.body,
  }).snapshot();
}
