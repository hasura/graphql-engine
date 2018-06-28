/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */

import { checkToggleButton } from './spec';

import { setMetaData } from '../../validators/validators';

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      // Visit the index route
      cy.visit('/data/schema/public');
      cy.wait(7000);
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const runMigrationModeTests = () => {
  describe('Migration mode', () => {
    it('Check the toggle button', checkToggleButton);
  });
};

// setup();
// runMigrationModeTests();
