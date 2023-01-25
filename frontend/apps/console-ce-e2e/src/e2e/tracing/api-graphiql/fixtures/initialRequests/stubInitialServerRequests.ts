import { config } from './config';
import { graphql } from './graphql';
import { export_metadata } from './export_metadata';
import { get_catalog_state } from './get_catalog_state';
import { get_inconsistent_metadata } from './get_inconsistent_metadata';

/**
 * For the sake of the tracing tests, the requests provided to the Console make no difference.
 * That's why all the initial requests are managed the quickest possible way, without caring,
 * for instance, that the Console performs a `version` request since it does not impact the
 * Console's life.
 */
export function stubInitialServerRequests() {
  cy.log('**--- Stub all the initial requests**');
  cy.intercept('http://localhost:8080/v1/metadata', req => {
    if (req.body.type === 'export_metadata') {
      req.alias = 'export_metadata';
      req.reply(export_metadata);
    }

    if (req.body.type === 'get_inconsistent_metadata') {
      req.alias = 'get_inconsistent_metadata';
      req.reply(get_inconsistent_metadata);
    }

    if (req.body.type === 'get_catalog_state') {
      req.alias = 'get_catalog_state';
      req.reply(get_catalog_state);
    }
  });

  cy.intercept('http://localhost:8080/v1alpha1/config', { body: config }).as(
    'config'
  );
  cy.intercept('http://localhost:8080/v1/graphql', { body: graphql }).as(
    'graphql'
  );
}

export function waitForInitialServerRequests() {
  // In no particular order...
  cy.wait('@config');
  cy.wait('@graphql');
  cy.wait('@export_metadata');
  cy.wait('@get_catalog_state');
  cy.wait('@get_inconsistent_metadata');
}
