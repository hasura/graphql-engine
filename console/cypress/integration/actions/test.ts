import {
  createMutationAction,
  modifyMutationAction,
  createQueryAction,
  modifyQueryAction,
  deleteMutationAction,
  deleteQueryAction,
} from './spec';
import { testMode } from '../../helpers/common';
import { setMetaData } from '../validators/validators';

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      cy.visit('/actions/manage/actions');
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const runActionsTests = () => {
  describe('Actions', () => {
    it('Create Mutation Action', createMutationAction);
    // it('Verify Mutation Actions on GraphiQL', verifyMutation);
    it('Modify Mutation Action', modifyMutationAction);
    it('Delete Mutation Action', deleteMutationAction);
    it('Create Query Action', createQueryAction);
    // it('Verify Query Actions on GraphiQL', verifyQuery);
    it('Modify Query Action', modifyQueryAction);
    it('Delete Query Action', deleteQueryAction);
  });
};

if (testMode !== 'cli') {
  setup();
  runActionsTests();
}
