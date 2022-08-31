import {
  passRTCreateTables,
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
import { getIndexRoute } from '../../../helpers/dataHelpers';

const setup = () => {
  // Temporarily skipped because of its flakiness, see: https://github.com/hasura/graphql-engine-mono/issues/5433
  // TODO: Fix and restore it
  describe.skip('Check Data Tab', () => {
    it('Clicking on Data tab opens the correct route', () => {
      cy.visit(getIndexRoute());
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const runRelationshipsTests = () => {
  // Temporarily skipped because of its flakiness, see: https://github.com/hasura/graphql-engine-mono/issues/5433
  // TODO: Fix and restore it
  describe.skip('Relationships Tests', () => {
    it('Create testing tables', passRTCreateTables);
    it('Add Manual Relationship Object', passRTAddManualObjRel);
    it('Add Manual Relationship Array', passRTAddManualArrayRel);
    it('Delete the relationships', passRTDeleteRelationships);
    it('Add Suggested Relationships Error', failRTAddSuggestedRel);
    it('Add Suggested Relationships', passRTAddSuggestedRel);
    it('Rename relationships', passRTRenameRelationship);
    it('Delete the relationships', passRTDeleteRelationships);
    it('Delete test tables', passRTDeleteTables);
  });
};

if (testMode !== 'cli') {
  setup();
  runRelationshipsTests();
}
