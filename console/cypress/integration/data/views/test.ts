import {
  passVCreateTables,
  passVCreateViews,
  passVAddData,
  passTrackTable,
  passVAddManualObjRel,
  passVAscendingSort,
  passVFilterQueryEq,
  passViewRoute,
  passModifyView,
  passVDeleteRelationships,
  passVDeleteView,
  passVDeleteTables,
} from './spec';
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

export const runViewsTest = () => {
  describe('Views', () => {
    // NOTE: Ideally, we should be adding "should" at the beginning of
    // the test descriptions. It will sound like this when you read it.
    // eg. it should create test tables ...and so on
    it('Create Tables', passVCreateTables);
    it('Insert test data to table(s)', passVAddData);
    it('Create View', passVCreateViews);
    it('Add View to comment table', passTrackTable);
    it('Visit the view route', passViewRoute);
    it('Order Ascending order View Table', passVAscendingSort);
    it('Apply Filters on the View', passVFilterQueryEq);
    it('Modify the View', passModifyView);
    it('Add Object Relationship to View', passVAddManualObjRel);
    it('Delete Relationship(s)', passVDeleteRelationships);
    it('Delete View', passVDeleteView);
    it('Delete Tables', passVDeleteTables);
  });
};

if (testMode !== 'cli') {
  setup();
  runViewsTest();
}
