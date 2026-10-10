import { getBaseUrl } from '../../../support/getBaseUrl';
import { hgeUrl } from '../../../support/endpoints';

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
  // Register the stub BEFORE visiting, so the page's initial
  // `get_inconsistent_metadata` request is reliably faked (the previous version
  // registered the intercept AFTER cy.visit, a race). All other /v1/metadata
  // calls are passed through to the real server with `req.continue()` so the
  // console still boots normally.
  cy.intercept('POST', hgeUrl('/v1/metadata'), (req) => {
    if (req?.body?.type === 'get_inconsistent_metadata') {
      req.reply(inconsistentMetadata);
      return;
    }
    req.continue();
  });

  cy.visit('/settings/metadata-status?is_redirected=true');

  const baseUrl = getBaseUrl();
  cy.url().should(
    'eq',
    `${baseUrl}/settings/metadata-status?is_redirected=true`,
  );

  // The inconsistent objects render in a DataTable (data-testid
  // `inconsistent-objects-table`) with combined cells: a "Name" column showing
  // the object type + definition, and a "Reason" column showing the reason +
  // message. Assert the faked row's content scoped to that table (the previous
  // per-cell selectors `[data-test=inconsistent_name_0` etc. were both malformed
  // — missing `]` — and obsolete after the table was rewritten).
  cy.get('[data-testid="inconsistent-objects-table"]').within(() => {
    cy.contains('DB2'); // definition
    cy.contains('source'); // type
    cy.contains('Inconsistent object: connection error'); // reason
    cy.contains(
      'could not translate host name "db" to address: Name or service not known',
    ); // message
  });
};
