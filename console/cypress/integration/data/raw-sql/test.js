/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */

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

const testPrefix = 'raw-sql';

const makeAssertion = (assertion, specFunc) => {
  it(`${testPrefix} : ${assertion}`, specFunc);
};

export const runRawSQLTests = () => {
  describe('Raw SQL', () => {
    makeAssertion('Open Raw SQL page', openRawSQL);
    makeAssertion('Pass create table', passCreateTable);
    makeAssertion('Pass insert values', passInsertValues);
    makeAssertion('Pass alter table', passAlterTable);
    makeAssertion('Pass create view', passCreateView);
    makeAssertion('Delete test table', delTestTables);
  });
};

if (testMode !== 'cli') {
  setup();
  runRawSQLTests();
}
