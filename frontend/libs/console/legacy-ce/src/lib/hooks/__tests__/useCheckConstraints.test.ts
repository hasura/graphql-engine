import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import {
  useDataSourceCheckConstraints,
  useTableCheckConstraints,
} from '../useCheckConstraints';
import { networkStubs } from './common/networkStubs';
import { APIError } from '../error';
import { wrapper } from './common/decorator';

const server = setupServer();

server.use(networkStubs.successWithData);
server.use(networkStubs.metadata);

beforeAll(() => server.listen());
afterAll(() => server.close());

describe("useCheckConstraints hooks' postgres test", () => {
  test('useTableCheckConstraints fetches data correctly', async () => {
    const { result, waitForValueToChange } = renderHook(
      () =>
        useTableCheckConstraints({
          table: { name: 'adults', schema: 'public' },
          driver: 'postgres',
          source: 'second',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);
    expect(result.current.data!.length).toBeGreaterThan(0);

    const firstResult = result.current.data![0];

    expect(firstResult.table_schema).toEqual('public');
    expect(firstResult.table_name).toEqual('adults');
    expect(firstResult.constraint_name).toEqual('adults_age_check');
    expect(firstResult.check).toEqual('CHECK (age > 18)');
  });

  test('useDataSourceCheckConstraints fetches data correctly', async () => {
    const schemas = ['public', 'export', 'books_schema', 'authors_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useDataSourceCheckConstraints({
          schemas,
          driver: 'postgres',
          source: 'second',
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
    expect(firstResult.constraint_name).toEqual('adults_age_check');
    expect(firstResult.table_name).toEqual('adults');
    expect(firstResult.check).toEqual('CHECK (age > 18)');
  });
});

describe("useCheckConstraints hooks' mssql test", () => {
  test('useTableCheckConstraints fetches data correctly', async () => {
    const { result, waitForValueToChange } = renderHook(
      () =>
        useTableCheckConstraints({
          table: { name: 'ms_Persons1', schema: 'dbo' },
          driver: 'mssql',
          source: 'mssql',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    expect(result.current.data!.length).toBeGreaterThan(0);

    const firstResult = result.current.data![0];

    expect(firstResult.table_schema).toEqual('dbo');
    expect(firstResult.table_name).toEqual('ms_Persons1');
    expect(firstResult.check).toEqual("([Age]>=(18) AND [City]='Sandnes')");
  });

  test('useDataSourceCheckConstraints fetches data correctly', async () => {
    const schemas = ['dbo', 'sales_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useDataSourceCheckConstraints({
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

    const secondResult = result.current.data![1];

    expect(secondResult.table_schema).toEqual('dbo');
    expect(secondResult.table_name).toEqual('ms_Persons2');
    expect(secondResult.check).toEqual('([Age]>=(18))');
    expect(secondResult.constraint_name).toEqual(
      'CK__ms_Persons2__Age__4BCC3ABA'
    );
  });
});

describe("useCheckConstraints hooks' error hanlding", () => {
  test('useTableCheckConstraints handles not-exists error', async () => {
    server.use(networkStubs.errorUnknownSource);
    const { result, waitForValueToChange } = renderHook(
      () =>
        useTableCheckConstraints(
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

  test('useDataSourceCheckConstraints handles inconsistent source error', async () => {
    server.use(networkStubs.errorWrongDriver);
    const schemas = ['dbo', 'sales_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useDataSourceCheckConstraints(
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
