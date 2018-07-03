/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */

import {
  passMTCheckRoute,
  passMTMoveToTable,
  passMTCreateTable,
  failMTWithoutColName,
  failMTWithoutColType,
  failMTDuplicateColumns,
  passMTAddTwoCols,
  passMTDeleteTableCancel,
  passMTDeleteCol,
  passMTChangeColType,
  passMTDeleteTable,
  failMTRemoveNullable,
  failMTWrongDefault,
} from './spec';

import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';

const setup = () => {
  describe('Check Data Tab', () => {
    it('Clicking on Data tab opens the correct route', () => {
      // Visit the index route
      cy.visit('/data/schema/public');
      cy.wait(7000);
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const runModifyTableTests = () => {
  describe('Modify Table', () => {
    it('Creating a table', passMTCreateTable);
    it('Moving to the table', passMTMoveToTable);
    it('Modify table button opens the correct route', passMTCheckRoute);
    it('Fails to add column without column name', failMTWithoutColName);
    it('Fails without type selected', failMTWithoutColType);
    it('Fail duplicate column', failMTDuplicateColumns);
    it('Fail for removing nullable after addition', failMTRemoveNullable);
    it('Fail with wrong default', failMTWrongDefault);
    it('Add 2 Columns', passMTAddTwoCols);
    it('Moving to column and change type', passMTChangeColType);
    it('Delete the column', passMTDeleteCol);
    it('Delete Table Cancel', passMTDeleteTableCancel);
    it('Delete table', passMTDeleteTable);
  });
};

if (testMode !== 'cli') {
  setup();
  runModifyTableTests();
}
