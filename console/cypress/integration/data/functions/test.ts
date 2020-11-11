import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';

import {
  createCustomFunctionSuccess,
  deleteCustomFunction,
  unTrackFunction,
  trackFunction,
  verifyPermissionTab,
  testSessVariable,
} from './spec';
import { getIndexRoute } from '../../../helpers/dataHelpers';

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      // Visit the index route
      cy.visit(getIndexRoute());
      cy.wait(5000);
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const runCreateCustomFunctionsTableTests = () => {
  describe('Custom Function Tests', () => {
    it('Create a custom function and track', createCustomFunctionSuccess);
    it('Untrack custom function', unTrackFunction);
    it('Track custom function', trackFunction);
    it('Verify permission tab', verifyPermissionTab);
    it('Delete custom function', deleteCustomFunction);
    it('Test custom function with Session Argument', testSessVariable);
  });
};

if (testMode !== 'cli') {
  setup();
  runCreateCustomFunctionsTableTests();
}
