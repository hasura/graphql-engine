import {
  createMutationAction,
  modifyMutationAction,
  createQueryAction,
  modifyQueryAction,
  deleteMutationAction,
  deleteQueryAction,
  createActionTransform,
  modifyActionTransform,
  deleteActionTransform,
  modifyV1ActionTransform,
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
    it('Create Action With Transform', createActionTransform);
    it('Update Action With Transform', modifyActionTransform);
    it('Delete Action With Transform', deleteActionTransform);
    it(
      'Create an action with V1 Transform and edit it through console, which will lead to the action being saved as V2',
      modifyV1ActionTransform
    );
  });
};

if (testMode !== 'cli') {
  setup();
  runActionsTests();
}
