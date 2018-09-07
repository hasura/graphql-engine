/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */

import {
  passRTCreateTables,
  passRTMoveToTable,
  passRTDeleteTables,
  passRTAddManualObjRel,
  passRTAddManualArrayRel,
  passRTAddForeignKey,
  passRTDeleteRelationships,
  passRTAddSuggestedRel,
  failRTAddSuggestedRel,
  checkAddManualRelationshipsButton,
} from './spec';
import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';

const setup = () => {
  describe('Check Data Tab', () => {
    it('Clicking on Data tab opens the correct route', () => {
      // Visit the index route
      cy.visit('/data/schema/public');
      cy.wait(7000);
      // Get and set validation metadata
      setMetaData();
    });
  });
};

const testPrefix = 'relationships';

const makeAssertion = (assertion, specFunc) => {
  it(`${testPrefix} : ${assertion}`, specFunc);
};

export const runRelationshipsTests = () => {
  describe('Relationships', () => {
    makeAssertion('Creating testing tables', passRTCreateTables);
    makeAssertion('Moving to the table', passRTMoveToTable);
    makeAssertion('Adding Manual Relationship Object', passRTAddManualObjRel);
    makeAssertion('Adding Manual Relationship Array', passRTAddManualArrayRel);
    makeAssertion('Adding a Foreign Key', passRTAddForeignKey);
    makeAssertion(
      'Check Add manual relationships button',
      checkAddManualRelationshipsButton
    );
    makeAssertion('Deleting the relationships', passRTDeleteRelationships);
    makeAssertion(
      'Adding Suggested Relationships Error',
      failRTAddSuggestedRel
    );
    makeAssertion('Adding Suggested Relationships', passRTAddSuggestedRel);
    makeAssertion('Deleting the relationships', passRTDeleteRelationships);
    makeAssertion('Deleting testing tables', passRTDeleteTables);
  });
};

if (testMode !== 'cli') {
  setup();
  runRelationshipsTests();
}
