import { setMetaData } from './validators/validators';

import { runMigrationModeTests } from './data/migration-mode/test';
import { runCreateTableTests } from './data/create-table/test';
import { runInsertBrowseTests } from './data/insert-browse/test';
import { runModifyTableTests } from './data/modify/test';
import { runPermissionsTests } from './data/permissions/test';
import { runRelationshipsTests } from './data/relationships/test';
import { runViewsTest } from './data/views/test';
import { runRawSQLTests } from './data/raw-sql/test';
import { run404Test } from './data/404/test';

import { runApiExplorerTests } from './api-explorer/graphql/test';

const setup = () => {
  it('Visit the index route', () => {
    // Wait for the server to start
    cy.wait(60000);
    // Visit the index route
    cy.visit('/data/schema/public');
    cy.wait(7000);
    // Get and set validation metadata
    setMetaData();
  });
};

describe('Setup route', setup);

runMigrationModeTests();

runCreateTableTests();

runInsertBrowseTests();

runModifyTableTests();

runRelationshipsTests();

runPermissionsTests();

runViewsTest();

runRawSQLTests();

runApiExplorerTests();

run404Test();
