/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */

import { testMode } from '../../../helpers/common';

import {
  passBICreateTable,
  deleteBITestTable,
  checkInsertRoute,
  failBIWrongDataType,
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

export const runInsertBrowseTests = () => {
  describe('Table: Browse and Insert', () => {
    it('Create a table with fields of all data types', passBICreateTable);
    it('Search for tables', passSearchTables);
    it('Check Insert Route', checkInsertRoute);
    it('Fails when entered wrong data type', failBIWrongDataType);
    it('Insert 20 rows', passBIInsert20Rows);
    it('Fail for adding same data for Unique keys', failBIUniqueKeys);
    it('Check browser rows route', checkBrowseRoute);
    it('20 Inserted rows reflect in browse rows', passBI20RowsExist);
    it('Check pagination in Browse Rows table', checkPagination);
    it('Ascending sort works as expected', () => passBISort('asc'));
    it('Descending sort works as expected', () => passBISort('desc'));
    it('Filter query works as expected with $eq', passBIFilterQueryEq);
    it('Check edit button', passEditButton);
    it('Check for clone clear', passCloneButton);
    it('Delete the row', passDeleteRow);
    it('Check view relationship', checkViewRelationship);
    it('Delete test table', deleteBITestTable);
  });
};

if (testMode !== 'cli') {
  setup();
  runInsertBrowseTests();
}
