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
import { getIndexRoute } from '../../../helpers/dataHelpers';

const setup = () => {
  // Temporarily skipped because of its flakiness, see: https://github.com/hasura/graphql-engine-mono/issues/5433
  // TODO: Fix and restore it
  describe.skip('Check Data Tab', () => {
    it('Visiting the data URL opens the correct route', () => {
      // Visit the index route
      cy.visit(getIndexRoute());
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const runPermissionsTests = () => {
  describe.skip('Permissions', () => {
    // Temporarily skipped because of its flakiness, see: https://github.com/hasura/graphql-engine-mono/issues/5433
    // TODO: Fix and restore it
    it('Create a table', passPTCreateTable);
    it('Create a view', passPVCreateView);
    it('Check permission route', passPTCheckRoute);
    it('Table No-check permissions work as expected', passPTNoChecks);
    it('Table Custom-check permissions work as expected', passPTCustomChecks);
    it('Table Permissions removal works as expected', passPTRemovePerms);
    it('View permissions work as expected', passPVPermissions);
    it('View Permissions removal works as expected', passPVRemovePerms);
    it('Delete the views', passPVDeleteView);
    it('Delete the test table', passPTDeleteTable);
  });
};

if (testMode !== 'cli') {
  setup();
  runPermissionsTests();
}
