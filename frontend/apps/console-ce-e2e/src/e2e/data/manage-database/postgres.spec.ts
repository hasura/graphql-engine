import { MetadataTable } from '@hasura/shared/types';
import { hgeUrl } from '../../../support/endpoints';
import { areTablesEqual } from '@hasura/metadata/helpers';
export const createTable = (tableName: string) => {
  const postBody = {
    type: 'run_sql',
    args: {
      source: 'default',
      sql: `CREATE TABLE "public"."${tableName}" ("id" serial NOT NULL, "name" text NOT NULL, "countryCode" text DEFAULT 'IN', PRIMARY KEY ("id") );`,
      cascade: false,
      read_only: false,
    },
  };
  cy.request('POST', hgeUrl('/v2/query'), postBody).then((response) => {
    expect(response.body).to.have.property('result_type', 'CommandOk'); // true
  });
};
export const deleteTable = (tableName: string) => {
  const postBody = {
    type: 'run_sql',
    args: {
      source: 'default',
      sql: `DROP table "public"."${tableName}";`,
      cascade: false,
      read_only: false,
    },
  };
  cy.request('POST', hgeUrl('/v2/query'), postBody).then((response) => {
    expect(response.body).to.have.property('result_type', 'CommandOk'); // true
  });
};

// Drop a table if it is still around, so a `before` hook's bare `CREATE TABLE`
// cannot fail with "relation already exists" when a previous suite's `after`
// cleanup left the table behind. `IF EXISTS` makes a MISSING table a no-op; this
// is NOT a blanket error-swallow — it still asserts `CommandOk`, so a real error
// (permissions, or a dependency that blocks a non-cascading drop) fails the
// setup loudly. It deliberately does NOT use SQL `CASCADE`: callers that may
// have a dependent object (e.g. an event trigger) must remove it first so the
// drop stays scoped to this fixture's own table.
export const dropTableIfExists = (tableName: string) => {
  const postBody = {
    type: 'run_sql',
    args: {
      source: 'default',
      sql: `DROP TABLE IF EXISTS "public"."${tableName}";`,
      cascade: false,
      read_only: false,
    },
  };
  cy.request('POST', hgeUrl('/v2/query'), postBody).then((response) => {
    expect(response.body).to.have.property('result_type', 'CommandOk');
  });
};

// Track a public table in the `default` source via the metadata API, and assert
// it is present in exported metadata so callers can rely on it being tracked
// before they navigate (a readiness check, not an arbitrary wait).
//
// This is DETERMINISTIC PREREQUISITE SETUP for specs that only need a tracked
// table to reach the permissions / event-trigger UI. It mirrors how the
// remote-schema specs set up their fixtures via `replace_metadata`. It is NOT a
// fix for the Data-manager UI tracking flow: on the hosted `test oss console`
// job these before-hooks' UI Track button (`track-<schema>.<table>`) did not
// appear, but that failure did not reproduce locally in either server- or
// CLI-mode, so its cause is unresolved. The feature under test (permissions /
// event triggers) is still exercised through the UI in each `it`; these
// before-hooks no longer exercise the UI tracking flow.
export const trackTable = (tableName: string) => {
  const postBody = {
    type: 'pg_track_table',
    args: {
      source: 'default',
      table: { name: tableName, schema: 'public' },
    },
  };
  cy.request('POST', hgeUrl('/v1/metadata'), postBody).then((response) => {
    expect(response.body).to.have.property('message', 'success');
  });
  // Readiness: confirm the table is actually tracked in the `default` source's
  // exported metadata before the caller navigates.
  cy.request('POST', hgeUrl('/v1/metadata'), {
    type: 'export_metadata',
    args: {},
  }).then((response) => {
    const source = (response.body.sources || []).find(
      (s: any) => s.name === 'default',
    );
    const isTracked = (source?.tables || []).some((t: MetadataTable) =>
      areTablesEqual(t.table, { name: tableName, schema: 'public' }),
    );
    expect(isTracked, `public.${tableName} tracked in source default`).to.be
      .true;
  });
};

type DriverSpec = {
  name: 'postgres';
  helpers: {
    createTable: (tableName: string) => void;
    deleteTable: (tableName: string) => void;
    dropTableIfExists: (tableName: string) => void;
    trackTable: (tableName: string) => void;
  };
};

const postgres: DriverSpec = {
  name: 'postgres',
  helpers: {
    createTable,
    deleteTable,
    dropTableIfExists,
    trackTable,
  },
};

export { postgres };
