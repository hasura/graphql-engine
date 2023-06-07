import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { useDataSourceUniqueKeys, useTableUniqueKeys } from '../useUniqueKeys';
import { networkStubs } from './common/networkStubs';
import { wrapper } from './common/decorator';
import { APIError } from '../error';

const server = setupServer();

server.use(networkStubs.successWithData);
server.use(networkStubs.metadata);

beforeAll(() => server.listen());
afterAll(() => server.close());

describe("useUniqueKeys hooks' postgres test", () => {
  test('useTableUniqueKeys fetches data correctly', async () => {
    const { result, waitForValueToChange } = renderHook(
      () =>
        useTableUniqueKeys({
          table: { name: 'car', schema: 'public' },
          driver: 'postgres',
          source: 'default',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    const firstResult = result.current.data![0];

    expect(firstResult.table_schema).toEqual('public');
    expect(firstResult.table_name).toEqual('car');
    expect(firstResult.constraint_name).toEqual('car_person_id_key');
    expect(firstResult.columns).toStrictEqual(['person_id']);
  });

  test('useDataSourceUniqueKeys fetches data correctly', async () => {
    const schemas = ['public', 'export', 'books_schema', 'authors_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useDataSourceUniqueKeys({
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
    expect(firstResult.table_schema).toEqual('public');
    expect(firstResult.table_name).toEqual('car');
    expect(firstResult.constraint_name).toEqual('car_person_id_key');
    expect(firstResult.columns).toStrictEqual(['person_id']);
  });
});

describe("useUniqueKeys hooks' mssql test", () => {
  test('useTableUniqueKeys fetches data correctly', async () => {
    const { result, waitForValueToChange } = renderHook(
      () =>
        useTableUniqueKeys({
          table: { name: 'citizens', schema: 'dbo' },
          driver: 'mssql',
          source: 'mssql',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    const firstResult = result.current.data![0];

    expect(firstResult.table_schema).toEqual('dbo');
    expect(firstResult.table_name).toEqual('citizens');
    expect(firstResult.constraint_name).toEqual('UC_Person');
    expect(firstResult.columns).toStrictEqual(['ID', 'LastName']);
  });

  test('useDataSourceUniqueKeys fetches data correctly', async () => {
    const schemas = ['dbo', 'sales_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useDataSourceUniqueKeys({
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
    expect(firstResult.table_name).toEqual('citizens');
    expect(firstResult.constraint_name).toEqual('UC_Person');
    expect(firstResult.columns).toStrictEqual(['ID', 'LastName']);
  });
});

describe("useUniqueKeys hooks' error hanlding", () => {
  test('useTableUniqueKeys handles not-exists error', async () => {
    server.use(networkStubs.errorUnknownSource);
    const { result, waitForValueToChange } = renderHook(
      () =>
        useTableUniqueKeys(
          {
            table: { name: 'car', schema: 'public' },
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

  test('useDataSourceUniqueKeys handles inconsistent source error', async () => {
    server.use(networkStubs.errorWrongDriver);
    const schemas = ['dbo', 'sales_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useDataSourceUniqueKeys(
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
