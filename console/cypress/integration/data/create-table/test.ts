import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';

import {
  checkCreateTableRoute,
  failCTWithoutColumns,
  failCTWithoutPK,
  failCTDuplicateColumns,
  failCTWrongDefaultValue,
  passCT,
  failCTDuplicateTable,
  deleteCTTestTables,
  passCTWithFK,
} from './spec';
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

export const runCreateTableTests = () => {
  describe('Create Table', () => {
    it('Create table button opens the correct route', checkCreateTableRoute);
    it('Fails to create table without columns', failCTWithoutColumns);
    it('Fails to create table without primary key', failCTWithoutPK);
    it('Fails to create with duplicate columns', failCTDuplicateColumns);
    it('Fails to create with wrong default value', failCTWrongDefaultValue);
    it('Successfuly creates table', passCT);
    it(
      'Successfuly creates table with composite foreign and unique key',
      passCTWithFK
    );
    it('Fails to create duplicate table', failCTDuplicateTable);
    it('Delete the test tables', deleteCTTestTables);
  });
};

if (testMode !== 'cli') {
  setup();
  runCreateTableTests();
}
