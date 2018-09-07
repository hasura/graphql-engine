/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */
import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';

import {
  checkCreateTableRoute,
  failCTWithoutColumns,
  failCTWithoutPK,
  failCTDuplicateColumns,
  failCTWrongDefaultValue,
  failCTDuplicatePrimaryKey,
  passCT,
  failCTDuplicateTable,
  failAddExistingTable,
  passAddExistingTable,
  deleteCTTestTable,
} from './spec';

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

const testPrefix = 'create-table';

const makeAssertion = (assertion, specFunc) => {
  it(`${testPrefix} : ${assertion}`, specFunc);
};

export const runCreateTableTests = () => {
  describe('Create Table', () => {
    makeAssertion(
      'Create table button opens the correct route',
      checkCreateTableRoute
    );
    makeAssertion(
      'Fails to create table without columns',
      failCTWithoutColumns
    );
    makeAssertion('Fails to create table without primary key', failCTWithoutPK);
    makeAssertion(
      'Fails to create with duplicate columns',
      failCTDuplicateColumns
    );
    makeAssertion(
      'Fails to create with duplicate primary key',
      failCTDuplicatePrimaryKey
    );
    makeAssertion(
      'Fails to create with wrong default value',
      failCTWrongDefaultValue
    );
    makeAssertion('Successfuly creates table', passCT);
    makeAssertion('Fails to add existing table', failAddExistingTable);
    makeAssertion('Passes add existing table', passAddExistingTable);
    makeAssertion('Fails to create duplicate table', failCTDuplicateTable);
    makeAssertion('Delete off the test table', deleteCTTestTable);
    makeAssertion(
      'Create table button opens the correct route',
      checkCreateTableRoute
    );
  });
};

if (testMode !== 'cli') {
  setup();
  runCreateTableTests();
}
