import { viewOnboarding, hideNow, dontShowAgain } from './spec';
import { testMode } from '../../helpers/common';
import { setMetaData } from '../validators/validators';
import { getIndexRoute } from '../../helpers/dataHelpers';

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      cy.visit(getIndexRoute());
      setMetaData();
    });
  });
};

export const runActionsTests = () => {
  describe('onboarding', () => {
    it('should show onboarding guide', viewOnboarding);
    it('should hide when user click on Hide Now', hideNow);
    it('should hide forever when user click on Dont Show again', dontShowAgain);
  });
};

if (testMode !== 'cli') {
  setup();
  runActionsTests();
}
