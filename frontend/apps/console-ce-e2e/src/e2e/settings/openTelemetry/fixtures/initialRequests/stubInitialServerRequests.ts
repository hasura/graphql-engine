import { config } from './config';
import { version } from './version';
import { export_metadata } from './export_metadata';
import { get_catalog_state } from './get_catalog_state';
import { get_inconsistent_metadata } from './get_inconsistent_metadata';

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
  cy.intercept('http://localhost:8080/v1/version', { body: version }).as(
    'version'
  );
}

export function waitForInitialServerRequests() {
  // In no particular order...
  cy.wait('@config');
  cy.wait('@version');
  cy.wait('@export_metadata');
  cy.wait('@get_catalog_state');
  cy.wait('@get_inconsistent_metadata');
}
