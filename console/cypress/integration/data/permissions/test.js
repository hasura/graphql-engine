/* eslint no-unused-vars: 0 */
/* eslint import/prefer-default-export: 0 */

import {
  passPTCreateTable,
  passPTCheckRoute,
  passPTNoChecks,
  passPTCustomChecks,
  passPTRemovePerms,
  passPVCreateView,
  passPVPermissions,
  passPVRemovePerms,
  passPVDeleteView,
  passPTDeleteTable,
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

const testPrefix = 'create-table';

const makeAssertion = (assertion, specFunc) => {
  it(`${testPrefix} : ${assertion}`, specFunc);
};

export const runPermissionsTests = () => {
  describe('Permissions', () => {
    makeAssertion('Create a table', passPTCreateTable);
    makeAssertion('Create a view', passPVCreateView);
    makeAssertion('Check permission route', passPTCheckRoute);
    makeAssertion(
      'Table No-check permissions work as expected',
      passPTNoChecks
    );
    makeAssertion(
      'Table Custom-check permissions work as expected',
      passPTCustomChecks
    );
    makeAssertion(
      'Table Permissions removal works as expected',
      passPTRemovePerms
    );
    makeAssertion('View permissions work as expected', passPVPermissions);
    makeAssertion(
      'View Permissions removal works as expected',
      passPVRemovePerms
    );
    makeAssertion('Delete off the views', passPVDeleteView);
    makeAssertion('Delete off the test table', passPTDeleteTable);
  });
};

if (testMode !== 'cli') {
  setup();
  runPermissionsTests();
}
