import { modifyV1ActionTransform } from './spec';
import { testMode } from '../../helpers/common';
import { setMetaData } from '../validators/validators';

const setup = () => {
  describe.skip('Setup route', () => {
    it('Visit the index route', () => {
      cy.visit('/actions/manage/actions');
      // Get and set validation metadata
      setMetaData();
    });
  });
};

// TODO: what about the codegen part? Why is it not tested?

export const runActionsTests = () => {
  describe.skip('Actions', () => {
    // The test has been moved to mutationAction.e2e.test
    // it('Create Mutation Action', createMutationAction);

    // The test was commented before moving the other ones to mutationAction.e2e.test
    // it('Verify Mutation Actions on GraphiQL', verifyMutation);

    // The test has been moved to mutationAction.e2e.test
    // it('Modify Mutation Action', modifyMutationAction);

    // The test has been moved to mutationAction.e2e.test
    // it('Delete Mutation Action', deleteMutationAction);

    // The test has been moved to queryAction.e2e.test.e2e.test
    // it('Create Query Action', createQueryAction);

    // The test was commented before moving the other ones to queryAction.e2e.test
    // it('Verify Query Actions on GraphiQL', verifyQuery);

    // The test has been moved to queryAction.e2e.test.e2e.test
    // it('Modify Query Action', modifyQueryAction);

    // The test has been moved to queryAction.e2e.test.e2e.test
    // it('Delete Query Action', deleteQueryAction);

    // The test has been moved to actionWithTransform.e2e.test.ts
    // it('Create Action With Transform', createActionTransform);

    // The test has been moved to actionWithTransform.e2e.test.ts
    // it('Update Action With Transform', modifyActionTransform);

    // The test has been moved to actionWithTransform.e2e.test.ts
    // it('Delete Action With Transform', deleteActionTransform);

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
