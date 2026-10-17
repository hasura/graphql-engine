import * as React from 'react';
import { renderHook, act } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import {
  runDatabaseMigration,
  runSQLBulk,
  useMigrationStatus,
} from '@hasura/metadata/api';
import { useAppContext } from '@hasura/shared/context';
import { useRunRawSQL } from './useRunRawSQL';

vi.mock('@hasura/metadata/api', () => ({
  // `getDownQueryComments` returns prebuilt run-sql query objects; the hook only
  // reads back `.args.sql`, so a minimal stub is enough to assert the mapping.
  getDownQueryComments: vi.fn(() => [{ args: { sql: '-- down comment' } }]),
  getTrackFunctionQuery: vi.fn(),
  getTrackTableArgs: vi.fn(),
  runDatabaseMigration: vi.fn(),
  runMetadataQuery: vi.fn(),
  runSQLBulk: vi.fn(),
  toastMetadataOutOfDateError: vi.fn(),
  useInvalidateMetadata: vi.fn(() => vi.fn()),
  useMigrationStatus: vi.fn(() => ({ refetch: vi.fn() })),
  // The CLI-server gate helper; these tests exercise the healthy-server path so
  // it is never invoked, but it must exist on the mock since it is imported.
  confirmCliServerUnavailable: vi.fn(() => Promise.resolve(true)),
}));

vi.mock('@hasura/shared/context', () => ({
  useAppContext: vi.fn(),
}));

vi.mock('@hasura/shared/hooks', () => ({
  useAuthFetchJson: vi.fn(() => vi.fn()),
}));

vi.mock('@hasura/shared/ui', () => ({
  hasuraToast: vi.fn(),
  showErrorNotification: vi.fn(),
  useHasuraAlert: vi.fn(() => ({
    hasuraAlert: vi.fn(),
    hasuraConfirm: vi.fn(),
    hasuraPrompt: vi.fn(),
  })),
}));

vi.mock('@hasura/metadata/data-source', () => ({
  // No `statementTimeoutSQL`/`parseCreateSchemaSQL` so the statement-timeout and
  // table-tracking branches are skipped and we isolate the migration path.
  getDatabaseMethods: vi.fn(() => ({ utilities: {} })),
  escapeTableColumnsMap: vi.fn(() => ({})),
  escapeTableName: vi.fn(() => undefined),
}));

const mockedRunDatabaseMigration = vi.mocked(runDatabaseMigration);
const mockedRunSQLBulk = vi.mocked(runSQLBulk);
const mockedUseMigrationStatus = vi.mocked(useMigrationStatus);
const mockedUseAppContext = vi.mocked(useAppContext);

const endpoints = {
  queryV2: '/v2/query',
  metadata: '/v1/metadata',
  hasuraCliServerMigrate: '/apis/migrate',
  hasuraCliServerMetadata: '/apis/metadata',
};

const setAppContext = ({
  consoleMode = 'cli',
  readOnlyMode = false,
}: { consoleMode?: string; readOnlyMode?: boolean } = {}) => {
  mockedUseAppContext.mockReturnValue({
    endpoints,
    envVars: { consoleMode },
    readOnlyMode,
  } as unknown as ReturnType<typeof useAppContext>);
};

const healthyCliServer = () =>
  mockedUseMigrationStatus.mockReturnValue({
    refetch: vi.fn().mockResolvedValue({ data: 'healthy' }),
  } as unknown as ReturnType<typeof useMigrationStatus>);

const wrapper = ({ children }: { children: React.ReactNode }) => (
  <QueryClientProvider client={new QueryClient()}>
    {children}
  </QueryClientProvider>
);

const postgresSource = { name: 'default', kind: 'postgres' };

describe('useRunRawSQL migration path', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mockedRunDatabaseMigration.mockResolvedValue([
      { result_type: 'CommandOk', result: [['ok']] },
    ] as never);
    mockedRunSQLBulk.mockResolvedValue([
      { result_type: 'CommandOk', result: [['ok']] },
    ] as never);
  });

  it('delegates CLI-mode migrations to runDatabaseMigration with raw SQL args (regression for pg_run_sql)', async () => {
    setAppContext({ consoleMode: 'cli' });
    healthyCliServer();

    const { result } = renderHook(() => useRunRawSQL(), { wrapper });

    await act(async () => {
      await result.current.mutateAsync({
        source: postgresSource,
        sql: 'CREATE TABLE foo (id int);',
        isTableTracked: false,
        cascade: false,
        readOnly: false,
        statementTimeout: 0,
        migration: { name: 'add_foo_table' },
      } as never);
    });

    // The CLI migration must go through runDatabaseMigration, not the raw bulk endpoint.
    expect(mockedRunSQLBulk).not.toHaveBeenCalled();
    expect(mockedRunDatabaseMigration).toHaveBeenCalledTimes(1);

    const callArg = mockedRunDatabaseMigration.mock.calls[0][0];
    expect(callArg.isMigration).toBe(true);
    expect(callArg.endpoints).toBe(endpoints);
    expect(callArg.args.name).toBe('add_foo_table');
    expect(callArg.args.source).toEqual(postgresSource);
    expect(callArg.args.up).toEqual([
      expect.objectContaining({
        sql: 'CREATE TABLE foo (id int);',
        readOnly: false,
      }),
    ]);
    // Regression: the up entry must be raw RunSqlArgs so runDatabaseMigration can
    // emit `run_sql`. Previously the hook prebuilt a `pg_run_sql` query object,
    // which the CLI migrate endpoint rejects.
    expect(callArg.args.up[0]).not.toHaveProperty('type');
    expect(callArg.args.down).toEqual([{ sql: '-- down comment' }]);
  });

  it('propagates the global readOnlyMode into the migration up args', async () => {
    setAppContext({ consoleMode: 'cli', readOnlyMode: true });
    healthyCliServer();

    const { result } = renderHook(() => useRunRawSQL(), { wrapper });

    await act(async () => {
      await result.current.mutateAsync({
        source: postgresSource,
        sql: 'SELECT 1;',
        isTableTracked: false,
        readOnly: false,
        statementTimeout: 0,
        migration: { name: 'read_only_migration' },
      } as never);
    });

    const callArg = mockedRunDatabaseMigration.mock.calls[0][0];
    expect(callArg.args.up[0].readOnly).toBe(true);
  });

  it('uses runSQLBulk (not a migration) when not in CLI mode even if a migration name is provided', async () => {
    setAppContext({ consoleMode: 'server' });

    const { result } = renderHook(() => useRunRawSQL(), { wrapper });

    await act(async () => {
      await result.current.mutateAsync({
        source: postgresSource,
        sql: 'SELECT 1;',
        isTableTracked: false,
        statementTimeout: 0,
        migration: { name: 'ignored_in_server_mode' },
      } as never);
    });

    expect(mockedRunDatabaseMigration).not.toHaveBeenCalled();
    expect(mockedRunSQLBulk).toHaveBeenCalledTimes(1);
    expect(mockedRunSQLBulk).toHaveBeenCalledWith(
      expect.objectContaining({ url: endpoints.queryV2, type: 'bulk' }),
    );
  });

  it('runs plain SQL through runSQLBulk when no migration is requested in CLI mode', async () => {
    setAppContext({ consoleMode: 'cli' });

    const { result } = renderHook(() => useRunRawSQL(), { wrapper });

    await act(async () => {
      await result.current.mutateAsync({
        source: postgresSource,
        sql: 'SELECT 1;',
        isTableTracked: false,
        statementTimeout: 0,
      } as never);
    });

    expect(mockedRunDatabaseMigration).not.toHaveBeenCalled();
    expect(mockedRunSQLBulk).toHaveBeenCalledTimes(1);
  });
});
