import {
  passRTCreateTables,
  passRTMoveToTable,
  passRTDeleteTables,
  passRTAddManualObjRel,
  passRTAddManualArrayRel,
  passRTDeleteRelationships,
  passRTAddSuggestedRel,
  failRTAddSuggestedRel,
  passRTRenameRelationship,
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

export const runRelationshipsTests = () => {
  describe('Relationships', () => {
    it('Creating testing tables', passRTCreateTables);
    it('Moving to the table', passRTMoveToTable);
    it('Adding Manual Relationship Object', passRTAddManualObjRel);
    it('Adding Manual Relationship Array', passRTAddManualArrayRel);
    it('Deleting the relationships', passRTDeleteRelationships);
    it('Adding Suggested Relationships Error', failRTAddSuggestedRel);
    it('Adding Suggested Relationships', passRTAddSuggestedRel);
    it('Rename relationships', passRTRenameRelationship);
    it('Deleting the relationships', passRTDeleteRelationships);
    it('Deleting testing tables', passRTDeleteTables);
  });
};

if (testMode !== 'cli') {
  setup();
  runRelationshipsTests();
}
