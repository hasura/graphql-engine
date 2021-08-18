import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';
import { getIndexRoute } from '../../../helpers/dataHelpers';

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      // Visit the index route
      cy.visit(getIndexRoute());
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const run404Test = () => {
  describe('404', () => {
    it('Open random page', () => {
      cy.visit('/someRandomPage');
      cy.get('h1').contains('404');
    });
  });
};

if (testMode !== 'cli') {
  setup();
  run404Test();
}
