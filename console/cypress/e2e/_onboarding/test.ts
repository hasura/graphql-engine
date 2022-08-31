import { viewOnboarding, hideNow, dontShowAgain } from './spec';
import { testMode } from '../../helpers/common';
import { setMetaData } from '../validators/validators';
import { getIndexRoute } from '../../helpers/dataHelpers';

const setup = () => {
  // Temporarily skipped because of its flakiness, see: https://github.com/hasura/graphql-engine-mono/issues/5433
  // TODO: Fix and restore it
  describe.skip('Setup route', () => {
    it('Visit the index route', () => {
      cy.visit(getIndexRoute());
      setMetaData();
    });
  });
};

export const runActionsTests = () => {
  describe('onboarding', () => {
    // Temporarily skipped because of its flakiness, see: https://github.com/hasura/graphql-engine-mono/issues/5433
    // TODO: Fix and restore it
    it.skip('should show onboarding guide', viewOnboarding);

    // Temporarily skipped because of its flakiness, see: https://github.com/hasura/graphql-engine-mono/issues/5433
    // TODO: Fix and restore it
    it.skip('should hide when user click on Hide Now', hideNow);

    // Temporarily skipped because of its flakiness, see: https://github.com/hasura/graphql-engine-mono/issues/5433
    // TODO: Fix and restore it
    it.skip(
      'should hide forever when user click on Dont Show again',
      dontShowAgain
    );
  });
};

if (testMode !== 'cli') {
  setup();
  runActionsTests();
}
