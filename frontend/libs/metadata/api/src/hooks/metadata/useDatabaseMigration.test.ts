import { Endpoints } from '@hasura/shared/context';
import { runDatabaseMigration } from './useDatabaseMigration';
import { MAX_ALLOWED_MIGRATION_LENGTH } from '../constants';

const postgresSource = { name: 'default', kind: 'postgres' };

const endpoints = {
  queryV2: '/v2/query',
  hasuraCliServerMigrate: '/apis/migrate',
} as unknown as Endpoints;

describe('runDatabaseMigration', () => {
  it('emits run_sql (not pg_run_sql) for up and down when isMigration is true', async () => {
    const fetchJson = vi.fn().mockResolvedValue([]);

    await runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration: true,
      args: {
        name: 'add_foo_table',
        up: [{ sql: 'CREATE TABLE foo (id int);', readOnly: false }],
        down: [{ sql: '-- down comment' }],
        source: postgresSource,
      },
    });

    expect(fetchJson).toHaveBeenCalledTimes(1);
    const [url, options] = fetchJson.mock.calls[0];
    expect(url).toBe('/apis/migrate');

    const body = JSON.parse(options.body);
    // The CLI migrate endpoint only accepts `run_sql`; the postgres driver would
    // otherwise produce `pg_run_sql`. This is the behavior the Raw SQL hook relies on.
    expect(body.up[0].type).toBe('run_sql');
    expect(body.down[0].type).toBe('run_sql');
    expect(body.up[0].args.sql).toBe('CREATE TABLE foo (id int);');
    expect(body.up[0].args.source).toBe('default');
    expect(body.down[0].args.sql).toBe('-- down comment');
    expect(body.datasource).toBe('default');
  });

  it.each([
    ['cockroach', 'cockroach_run_sql'],
    ['citus', 'citus_run_sql'],
    ['mssql', 'mssql_run_sql'],
  ] as const)(
    'keeps the %s prefix for up and down when isMigration is true',
    async (kind, expectedType) => {
      const fetchJson = vi.fn().mockResolvedValue([]);

      await runDatabaseMigration({
        endpoints,
        fetchJson,
        isMigration: true,
        args: {
          name: 'add_foo_table',
          up: [{ sql: 'CREATE TABLE foo (id int);' }],
          down: [{ sql: 'DROP TABLE foo;' }],
          source: { name: 'db', kind },
        },
      });

      const body = JSON.parse(fetchJson.mock.calls[0][1].body);
      // an unprefixed `run_sql` would be run as `pg_run_sql` on this source
      expect(body.up[0].type).toBe(expectedType);
      expect(body.down[0].type).toBe(expectedType);
    },
  );

  it('sanitizes and truncates the migration name', async () => {
    const fetchJson = vi.fn().mockResolvedValue([]);
    const unsafeLongName = `evil/../name${'x'.repeat(300)}`;

    await runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration: true,
      args: {
        name: unsafeLongName,
        up: [{ sql: 'SELECT 1;' }],
        down: [],
        source: postgresSource,
      },
    });

    const body = JSON.parse(fetchJson.mock.calls[0][1].body);
    expect(body.name.length).toBeLessThanOrEqual(MAX_ALLOWED_MIGRATION_LENGTH);
    expect(body.name).not.toContain('/');
  });

  it('runs a bulk run-sql against v2/query when isMigration is false', async () => {
    const fetchJson = vi.fn().mockResolvedValue([]);

    await runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration: false,
      args: {
        name: 'ignored',
        up: [{ sql: 'SELECT 1;' }],
        down: [],
        source: postgresSource,
      },
    });

    expect(fetchJson).toHaveBeenCalledTimes(1);
    const [url, options] = fetchJson.mock.calls[0];
    expect(url).toBe('/v2/query');

    const body = JSON.parse(options.body);
    expect(body.type).toBe('bulk');
    // /v2/query accepts the driver-prefixed type, unlike the CLI migrate endpoint.
    expect(body.args[0].type).toBe('pg_run_sql');
    expect(body.args[0].args.sql).toBe('SELECT 1;');
  });
});
