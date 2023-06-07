import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { useDataSourceTables, useSingleTable } from '../useTables';
import { networkStubs } from './common/networkStubs';
import { wrapper } from './common/decorator';
import { APIError } from '../error';

const server = setupServer();

server.use(networkStubs.successWithData);
server.use(networkStubs.metadata);

beforeAll(() => server.listen());
afterAll(() => server.close());

describe("useTables hooks' postgres test", () => {
  test('useSingleTable fetches data correctly', async () => {
    const { result, waitForValueToChange } = renderHook(
      () =>
        useSingleTable({
          table: { name: 'person', schema: 'public' },
          driver: 'postgres',
          source: 'default',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    const firstResult = result.current.data!;

    expect(firstResult.table_schema).toEqual('public');
    expect(firstResult.table_name).toEqual('person');
    expect(firstResult.is_table_tracked).toEqual(true);
  });

  test('useDataSourceTables fetches data correctly', async () => {
    const schemas = ['public', 'export', 'books_schema', 'authors_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useDataSourceTables({
          schemas,
          driver: 'postgres',
          source: 'default',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.data);

    expect(result.current.data!.length).toBeGreaterThan(0);

    result.current.data!.forEach(table => {
      expect(schemas).toContain(table.table_schema);
    });

    const firstResult = result.current.data![0];
    expect(firstResult.table_schema).toEqual('authors_schema');
    expect(firstResult.table_name).toEqual('authors');
    expect(firstResult.comment).toEqual(null);
  });
});

describe("useTables hooks' mssql test", () => {
  test('useSingleTable fetches data correctly', async () => {
    const { result, waitForValueToChange } = renderHook(
      () =>
        useSingleTable({
          table: { name: 'flights', schema: 'dbo' },
          driver: 'mssql',
          source: 'mssql',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    const firstResult = result.current.data!;

    expect(firstResult.table_schema).toEqual('dbo');
    expect(firstResult.table_name).toEqual('flights');
    expect(firstResult.is_table_tracked).toEqual(true);
  });

  test('useDataSourceTables fetches data correctly', async () => {
    const schemas = ['dbo', 'sales_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useDataSourceTables({
          schemas,
          driver: 'mssql',
          source: 'mssql',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.data);

    expect(result.current.data!.length).toBeGreaterThan(0);

    result.current.data!.forEach(table => {
      expect(schemas).toContain(table.table_schema);
    });

    const firstResult = result.current.data![0];
    expect(firstResult.table_schema).toEqual('dbo');
    expect(firstResult.table_name).toEqual('flights');
    expect(firstResult.comment).toEqual(undefined);
  });
});

describe("useTables hooks' error hanlding", () => {
  test('useSingleTable handles not-exists error', async () => {
    server.use(networkStubs.errorUnknownSource);
    const { result, waitForValueToChange } = renderHook(
      () =>
        useSingleTable(
          {
            table: { name: 'person', schema: 'public' },
            driver: 'postgres',
            source: 'secon',
          },
          { retry: 0 }
        ),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isError);

    const error = result.current.error!;

    expect(error.message).toContain('source with name "secon" does not exist');
    expect(error.code).toEqual('not-exists');
    expect(error instanceof APIError);
  });

  test('useDataSourceTables handles inconsistent source error', async () => {
    server.use(networkStubs.errorWrongDriver);
    const schemas = ['dbo', 'sales_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useDataSourceTables(
          {
            schemas,
            driver: 'mssql',
            source: 'default',
          },
          { retry: 0 }
        ),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.error);

    const error = result.current.error!;
    expect(error.message).toContain(
      'source with name "default" is inconsistent'
    );
    expect(error.code).toEqual('unexpected');
    expect(error instanceof APIError);
  });
});
