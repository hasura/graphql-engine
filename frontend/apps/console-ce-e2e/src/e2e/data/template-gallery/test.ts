import { getDbRoute } from '../../../helpers/dataHelpers';
import { setMetaData } from '../../validators/validators';
import { setPromptValue, testMode } from '../../../helpers/common';

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      cy.visit(getDbRoute());
      cy.wait(7000);
      setMetaData();
    });
  });
};

export const runSchemaSharingTests = () => {
  describe('template gallery', () => {
    it('display content', () => {
      cy.get('[data-test=table-links]').contains('default').click();
      cy.get('table').contains('Relationships: One-to-One').click();
      cy.contains('Install Template').click();
      cy.wait(1000);
      cy.get('[data-test=table-links]').contains('_onetoone').click();
      setPromptValue('_onetoone');
      cy.contains('_onetoone').parent().parent().contains('owner');
      cy.contains('_onetoone').parent().parent().contains('passport_info');
      cy.get('[title="Delete current schema"]').click();
    });
  });
};

if (testMode !== 'cli') {
  setup();
  runSchemaSharingTests();
}
