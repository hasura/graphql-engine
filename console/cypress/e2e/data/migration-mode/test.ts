import { checkToggleButton } from './spec';
import { testMode } from '../../../helpers/common';

import { setMetaData } from '../../validators/validators';
import { getIndexRoute } from '../../../helpers/dataHelpers';

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      // Visit the index route
      cy.visit(getIndexRoute());
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
