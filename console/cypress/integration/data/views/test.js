/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */

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

const testPrefix = 'views';

const makeAssertion = (assertion, specFunc) => {
  it(`${testPrefix} : ${assertion}`, specFunc);
};

export const runViewsTest = () => {
  describe('Views', () => {
    makeAssertion('Create Tables', passVCreateTables);
    makeAssertion('Add data to table', passVAddData);
    makeAssertion('Create View', passVCreateViews);
    makeAssertion('Adding it to the table', passTrackTable);
    makeAssertion('Check the view route', passViewRoute);
    makeAssertion('Ascending order View Table', passVAscendingSort);
    makeAssertion('Filter the View table', passVFilterQueryEq);
    makeAssertion('Modify the View', passModifyView);
    makeAssertion('Adding Object Relationship to View', passVAddManualObjRel);
    makeAssertion('Deleting Relationship', passVDeleteRelationships);
    makeAssertion('Deleting View', passVDeleteView);
    makeAssertion('Deleting Tables', passVDeleteTables);
  });
};

if (testMode !== 'cli') {
  setup();
  runViewsTest();
}
