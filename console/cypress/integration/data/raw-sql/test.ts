import {
  openRawSQL,
  passCreateTable,
  delTestTables,
  passCreateView,
  passInsertValues,
  passAlterTable,
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

export const runRawSQLTests = () => {
  describe('Raw SQL', () => {
    it('Open Raw SQL page', openRawSQL);
    it('Pass create table', passCreateTable);
    it('Pass insert values', passInsertValues);
    it('Pass alter table', passAlterTable);
    it('Pass create view', passCreateView);
    it('Delete test table', delTestTables);
  });
};

if (testMode !== 'cli') {
  setup();
  runRawSQLTests();
}
