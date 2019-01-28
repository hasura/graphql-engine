/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */
import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';

import {
  openRawSQL,
  createCustomFunctionSuccess,
  deleteCustomFunction,
  unTrackFunction,
  trackFunction,
  verifyPermissionTab,
} from './spec';

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      // Visit the index route
      cy.visit('/data');
      cy.wait(5000);
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const runCreateCustomFunctionsTableTests = () => {
  describe('Create Custom Function', () => {
    // it(
    //   'Visit Run SQL page',
    //   openRawSQL,
    // );
    it('Create a custom function and track', createCustomFunctionSuccess);
    it('Untrack custom function', unTrackFunction);
    it('Track custom function', trackFunction);
    it('Verify permission tab', verifyPermissionTab);
    it('Delete custom function', deleteCustomFunction);
  });
};

if (testMode !== 'cli') {
  setup();
  runCreateCustomFunctionsTableTests();
}
