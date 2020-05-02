/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */

import {
  passVCreateTables,
  passVCreateMaterializedViews,
  passVAddData,
  passTrackTable,
  passVAddManualObjRel,
  passVAscendingSort,
  passModifyMaterializedView,
  passVFilterQueryEq,
  passMaterializedViewRoute,
  passVDeleteRelationships,
  passVDeleteMaterializedView,
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

export const runMaterializedViewsTest = () => {
  describe('Materialized Views', () => {
    it('Create Tables', passVCreateTables);
    it('Add data to table', passVAddData);
    it('Create MaterializedView', passVCreateMaterializedViews);
    it('Adding it to the table', passTrackTable);
    it('Check the materializedview route', passMaterializedViewRoute);
    it('Ascending order MaterializedView Table', passVAscendingSort);
    it('Filter the MaterializedView table', passVFilterQueryEq);
    it('Modify the View', passModifyMaterializedView);
    it('Adding Object Relationship to MaterializedView', passVAddManualObjRel);
    it('Deleting Relationship', passVDeleteRelationships);
    it('Deleting MaterializedView', passVDeleteMaterializedView);
    it('Deleting Tables', passVDeleteTables);
  });
};

if (testMode !== 'cli') {
  setup();
  runMaterializedViewsTest();
}
