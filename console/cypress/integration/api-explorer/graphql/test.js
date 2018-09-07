/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */

import {
  openAPIExplorer,
  checkExecuteQueryButton,
  checkQuery,
  checkMutation,
  createTestTable,
  insertValue,
  checkSub,
  delTestTable,
} from './spec';

import { setMetaData } from '../../validators/validators';
import { testMode } from '../../../helpers/common';

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      // Visit the index route
      cy.visit('/');
      cy.wait(7000);
      // Get and set validation metadata
      setMetaData();
    });
  });
};

const testPrefix = 'api-explorer';

const makeAssertion = (assertion, specFunc) => {
  it(`${testPrefix} : ${assertion}`, specFunc);
};

export const runApiExplorerTests = () => {
  describe('API Explorer', () => {
    makeAssertion('Create test table', createTestTable);
    makeAssertion('Insert row into test table', insertValue);
    makeAssertion('Open API Explorer', openAPIExplorer);
    // it('Check Run Query button', checkExecuteQueryButton);
    makeAssertion('Check query result', checkQuery);
    makeAssertion('Check mutation result', checkMutation);
    makeAssertion('Check subscription result', checkSub);
    makeAssertion('Delete test table', delTestTable);
  });
};

if (testMode !== 'cli') {
  setup();
  runApiExplorerTests();
}
