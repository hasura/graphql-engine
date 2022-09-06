import { getElementFromAlias, baseUrl } from '../../../helpers/dataHelpers';

// fake inconsistentMetadata api response
const inconsistentMetadata = {
  is_consistent: false,
  inconsistent_objects: [
    {
      definition: 'DB2',
      reason: 'Inconsistent object: connection error',
      name: 'source DB2',
      type: 'source',
      message:
        'could not translate host name "db" to address: Name or service not known\n',
    },
  ],
};

export const inconsistentMetadataPage = () => {
  // Set first column
  cy.visit('/settings/metadata-status?is_redirected=true');
  cy.intercept('/v1/metadata', req => {
    // dynamically respond to a request here

    if (req?.body?.type === 'get_inconsistent_metadata') {
      // fake inconsistentMetadata api response to test the UI
      return req.reply(inconsistentMetadata);
    }

    // send all other requests to the destination server
    req.reply();
  });
  cy.url().should(
    'eq',
    `${baseUrl}/settings/metadata-status?is_redirected=true`
  );

  cy.get(getElementFromAlias('inconsistent_name_0')).contains('DB2');
  cy.get(getElementFromAlias('inconsistent_type_0')).contains('source');
  cy.get(getElementFromAlias('inconsistent_reason_0')).contains(
    'Inconsistent object: connection error'
  );
  cy.get(getElementFromAlias('inconsistent_reason_0')).contains(
    'could not translate host name "db" to address: Name or service not known'
  );
};
