interface SingleMetadataRequest {
  type: string;
  // There are a lot of other fields, but tracking them is not important for the purpose of this module
}

interface BulkMetadataRequest {
  type: 'bulk';
  args: SingleMetadataRequest[];
}

type MetadataRequest = SingleMetadataRequest | BulkMetadataRequest;

/*
 * Log all the requests outgoing to the Metadata endpoint.
 * This is useful to have a glance of the requests that are going to the server.
 */
export function logMetadataRequests() {
  cy.intercept('POST', 'http://localhost:8080/v1/metadata', req => {
    const noArgs = !req.body.args;

    if (noArgs) return;

    const requestBody = req.body as MetadataRequest;

    if (requestBody.type === 'bulk' || requestBody.type === 'concurrent_bulk') {
      const request = requestBody as BulkMetadataRequest;
      Cypress.log({ message: '*--- Bulk request*' });

      request.args.forEach(arg =>
        Cypress.log({ message: `*--- Request: ${arg.type}*` })
      );
    } else {
      Cypress.log({ message: `*--- Request: ${requestBody.type}*` });
    }
  });
}
