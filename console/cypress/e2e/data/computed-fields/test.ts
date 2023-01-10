import {
  openRawSQL,
  createTableAuthor,
  createCustomFunction,
  insertAuthorsIntoTable,
  searchForTable,
  cleanUpSql,
  openModifySection,
  routeToGraphiql,
  verifyComputedFieldsResult,
  routeToSQLPage,
} from './spec';
import { testMode } from '../../../helpers/common';
import { setMetaData } from '../../validators/validators';
import { getIndexRoute } from '../../../helpers/dataHelpers';

const setup = () => {
  describe('Setup route', () => {
    it('Visit the index route', () => {
      cy.visit(getIndexRoute());
      // Get and set validation metadata
      setMetaData();
    });
  });
};

export const runComputedFieldTests = () => {
  describe('Computed Fields', () => {
    it('Open Raw SQL page', openRawSQL);
    it('Create test table', createTableAuthor);
    it('Run SQL for custom function', createCustomFunction);
    it('Insert authors into table', insertAuthorsIntoTable);
    it('Search for table', searchForTable);

    // Temporarily skipped because of its flakiness, see: https://github.com/hasura/graphql-engine-mono/issues/5433
    // TODO: Fix and restore them
    it.skip('Open Modify page and add computed field', openModifySection);
    it.skip('Route to GraphiQL page', routeToGraphiql);
    it.skip(
      'Check computed field results on GraphiQL',
      verifyComputedFieldsResult
    );
    it.skip('Route to Raw SQL page', routeToSQLPage);
    it.skip('Test cleanup', cleanUpSql);
  });
};

if (testMode !== 'cli') {
  setup();
  runComputedFieldTests();
}
