/* eslint import/prefer-default-export: 0 */

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

const testPrefix = 'modify-table';

const makeAssertion = (assertion, specFunc) => {
  it(`${testPrefix} : ${assertion}`, specFunc);
};

export const runModifyTableTests = () => {
  describe('Modify Table', () => {
    makeAssertion('Creating a table', passMTCreateTable);
    makeAssertion('Moving to the table', passMTMoveToTable);
    makeAssertion(
      'Modify table button opens the correct route',
      passMTCheckRoute
    );
    makeAssertion(
      'Fails to add column without column name',
      failMTWithoutColName
    );
    makeAssertion('Fails without type selected', failMTWithoutColType);
    makeAssertion('Add a column', passMTAddColumn);
    makeAssertion(
      'Fail modify with wrong default value',
      failMCWithWrongDefaultValue
    );
    makeAssertion(
      'Pass modify with wrong default value',
      passMCWithRightDefaultValue
    );
    makeAssertion('Pass create foreign-key', passCreateForeignKey);
    makeAssertion('Pass remove foreign-key', passRemoveForeignKey);
    makeAssertion('Delete the column', passMTDeleteCol);
    makeAssertion('Delete Table Cancel', passMTDeleteTableCancel);
    makeAssertion('Delete table', passMTDeleteTable);
  });
};

if (testMode !== 'cli') {
  setup();
  runModifyTableTests();
}
