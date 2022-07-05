import {
  openAPIExplorer,
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
