import { checkToggleButton } from './spec';
import { testMode } from '../../../helpers/common';

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

if (testMode !== 'cli') {
  setup();
  runMigrationModeTests();
}
