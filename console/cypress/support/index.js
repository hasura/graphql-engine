// ***********************************************************
// This example support/index.js is processed and
// loaded automatically before your test files.
//
// This is a great place to put global configuration and
// behavior that modifies Cypress.
//
// You can change the location of this file or turn off
// automatically serving support files with the
// 'supportFile' configuration option.
//
// You can read more here:
// https://on.cypress.io/configuration
// ***********************************************************

// Import commands.js using ES2015 syntax:
import './commands';

const istanbul = require('istanbul-lib-coverage');

const map = istanbul.createCoverageMap({});

Cypress.on('window:before:unload', e => {
  const coverage = e.currentTarget.__coverage__;

  if (coverage) {
    map.merge(coverage);
  }
});

after(() => {
  cy.window().then(win => {
    const coverage = win.__coverage__;

    if (coverage) {
      map.merge(coverage);
    }

    cy.writeFile('.nyc_output/out.json', JSON.stringify(map));
    cy.exec('nyc report --reporter=html');
  });
});
