/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */

import { openRawSQL } from './spec';
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

export const runRawSQLTests = () => {
  describe('Raw SQL', () => {
    it('Open Raw SQL page', openRawSQL);
  });
};

if (testMode !== 'cli') {
  setup();
  runRawSQLTests();
}
