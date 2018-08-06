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

export const runApiExplorerTests = () => {
  describe('API Explorer', () => {
    it('Create test table', createTestTable);
    it('Insert row into test table', insertValue);
    it('Open API Explorer', openAPIExplorer);
    // it('Check Run Query button', checkExecuteQueryButton);
    it('Check query result', checkQuery);
    it('Check mutation result', checkMutation);
    it('Check subscription result', checkSub);
    it('Delete test table', delTestTable);
  });
};

if (testMode !== 'cli') {
  setup();
  runApiExplorerTests();
}
