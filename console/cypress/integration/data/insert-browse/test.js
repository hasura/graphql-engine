/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */

import { testMode } from '../../../helpers/common';

import {
  passBICreateTable,
  deleteBITestTable,
  checkInsertRoute,
  failBIWrongDataType,
  failBINullKeys,
  failBIUniqueKeys,
  passBIInsert20Rows,
  checkBrowseRoute,
  passBI20RowsExist,
  checkPagination,
  passBISort,
  passBIFilterQueryEq,
  passEditButton,
  passSearchTables,
  passCloneButton,
  checkViewRelationship,
  passDeleteRow,
} from './spec';

import { setMetaData } from '../../validators/validators';

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      cy.visit('/data/schema/public');
      cy.wait(7000);
      setMetaData();
    });
  });
};

const testPrefix = 'insert-browse';

const makeAssertion = (assertion, specFunc) => {
  it(`${testPrefix} : ${assertion}`, specFunc);
};

export const runInsertBrowseTests = () => {
  describe('Table: Browse and Insert', () => {
    makeAssertion(
      'Create a table with fields of all data types',
      passBICreateTable
    );
    makeAssertion('Search for tables', passSearchTables);
    makeAssertion('Check Insert Route', checkInsertRoute);
    makeAssertion('Fails when entered wrong data type', failBIWrongDataType);
    makeAssertion('Insert 20 rows', passBIInsert20Rows);
    makeAssertion('Fail for adding null for not null keys', failBINullKeys);
    makeAssertion(
      'Fail for adding same data for Unique keys',
      failBIUniqueKeys
    );
    makeAssertion('Check browser rows route', checkBrowseRoute);
    makeAssertion('20 Inserted rows reflect in browse rows', passBI20RowsExist);
    makeAssertion('Check pagination in Browse Rows table', checkPagination);
    makeAssertion('Ascending sort works as expected', () => passBISort('asc'));
    makeAssertion('Descending sort works as expected', () =>
      passBISort('desc')
    );
    makeAssertion(
      'Filter query works as expected with $eq',
      passBIFilterQueryEq
    );
    makeAssertion('Check edit button', passEditButton);
    makeAssertion('Check for clone clear', passCloneButton);
    makeAssertion('Delete the row', passDeleteRow);
    makeAssertion('Check view relationship', checkViewRelationship);
    makeAssertion('Delete test table', deleteBITestTable);
  });
};

if (testMode !== 'cli') {
  setup();
  runInsertBrowseTests();
}
