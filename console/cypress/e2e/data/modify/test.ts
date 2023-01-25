import {
  passMTCheckRoute,
  passMTMoveToTable,
  passMTCreateTable,
  failMTWithoutColName,
  failMTWithoutColType,
  passMTAddColumn,
  passMTDeleteTableCancel,
  passMTDeleteCol,
  passMTDeleteTable,
  passMCWithRightDefaultValue,
  failMCWithWrongDefaultValue,
  passCreateForeignKey,
  passRemoveForeignKey,
  passMTRenameTable,
  passMTRenameColumn,
  passModifyPkey,
  passCreateUniqueKey,
  passModifyUniqueKey,
  passRemoveUniqueKey,
  passMTChangeDefaultValueForPKey,
  passMTFunctionList,
} from './spec';

import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';
import { getIndexRoute } from '../../../helpers/dataHelpers';

const setup = () => {
  describe('Check Data Tab', () => {
    it('Clicking on Data tab opens the correct route', () => {
      // Visit the index route
      cy.visit(getIndexRoute());
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
    it(
      'Can create computed field with compatible functions',
      passMTFunctionList
    );
    it('Pass renaming table', passMTRenameTable);
    it('Pass renaming column', passMTRenameColumn);
    it('Fails to add column without column name', failMTWithoutColName);
    it('Fails without type selected', failMTWithoutColType);
    it('Add a column', passMTAddColumn);
    it('Fail modify with wrong default value', failMCWithWrongDefaultValue);
    it('Pass modify with wrong default value', passMCWithRightDefaultValue);
    it('Pass create foreign-key', passCreateForeignKey);
    it('Pass remove foreign-key', passRemoveForeignKey);
    it(
      'Pass edit default value for primary key',
      passMTChangeDefaultValueForPKey
    );
    it('Pass modifying a primary key', passModifyPkey);
    it('Pass creating a unique key', passCreateUniqueKey);
    it('Pass modifying a unique key', passModifyUniqueKey);
    it('Pass removing a unique key', passRemoveUniqueKey);
    it('Delete the column', passMTDeleteCol);
    it('Delete Table Cancel', passMTDeleteTableCancel);
    it('Delete table', passMTDeleteTable);
  });
};

if (testMode !== 'cli') {
  setup();
  runModifyTableTests();
}
