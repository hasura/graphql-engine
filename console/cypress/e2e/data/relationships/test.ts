import {
  passRTCreateTables,
  passRTDeleteTables,
  passRTAddManualObjRel,
  passRTAddManualArrayRel,
  passRTDeleteRelationships,
  passRTAddSuggestedRel,
  failRTAddSuggestedRel,
  passRTRenameRelationship,
  passRSTAddRSRel,
  passRSTDeleteRSRel,
  passRSTSetup,
  passRSTReset,
} from './spec';
import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';
import { getIndexRoute } from '../../../helpers/dataHelpers';
import { postgres } from '../manage-database/postgres.spec';

const setup = () => {
  describe('Check Data Tab', () => {
    it('Clicking on Data tab opens the correct route', () => {
      cy.visit(getIndexRoute());
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const runRelationshipsTests = () => {
  describe('Relationships Tests', () => {
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

export const remoteRelationshipTests = () => {
  const drivers = [postgres];

  describe('Remote schema relationships tests', () => {
    drivers.forEach(driver => {
      describe(`for ${driver.name}`, () => {
        // test setup
        before(() => {
          driver.helpers.createRemoteSchema('remote_rel_test_rs');
        });

        it('Create testing tables', passRSTSetup);
        it('Adds a relationship', passRSTAddRSRel);
        it('Deletes a relationship', passRSTDeleteRSRel);
        it('Delete testing tables', passRSTReset);

        // clean up
        after(() => {
          driver.helpers.deleteRemoteSchema('remote_rel_test_rs');
        });
      });
    });
  });
};

if (testMode !== 'cli') {
  setup();
  runRelationshipsTests();
  remoteRelationshipTests();
}
