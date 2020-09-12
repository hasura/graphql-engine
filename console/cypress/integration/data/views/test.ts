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

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      // Visit the index route
      cy.visit('/data/schema/public');
      cy.wait(7000);
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const runViewsTest = () => {
  describe('Views', () => {
    it('Create Tables', passVCreateTables);
    it('Add data to table', passVAddData);
    it('Create View', passVCreateViews);
    it('Adding it to the table', passTrackTable);
    it('Check the view route', passViewRoute);
    it('Ascending order View Table', passVAscendingSort);
    it('Filter the View table', passVFilterQueryEq);
    it('Modify the View', passModifyView);
    it('Adding Object Relationship to View', passVAddManualObjRel);
    it('Deleting Relationship', passVDeleteRelationships);
    it('Deleting View', passVDeleteView);
    it('Deleting Tables', passVDeleteTables);
  });
};

if (testMode !== 'cli') {
  setup();
  runViewsTest();
}
