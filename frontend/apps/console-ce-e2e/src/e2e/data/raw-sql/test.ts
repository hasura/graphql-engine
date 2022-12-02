import {
  openRawSQL,
  passCreateTable,
  delTestTables,
  passCreateView,
  passInsertValues,
  passAlterTable,
  readQuery,
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

export const runRawSQLTests = () => {
  describe('Raw SQL', () => {
    it('Open Raw SQL page', openRawSQL);
    it('Pass create table', passCreateTable);
    it('Pass insert values', passInsertValues);
    it('Pass alter table', passAlterTable);
    it('Read from table', readQuery);
    it('Pass create view', passCreateView);
    it('Delete test table', delTestTables);
  });
};

if (testMode !== 'cli') {
  setup();
  runRawSQLTests();
}
