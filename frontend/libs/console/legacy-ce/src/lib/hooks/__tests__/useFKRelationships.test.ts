import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import {
  useDataSourceFKRelationships,
  useTableFKRelationships,
} from '../useFKRelationships';
import { networkStubs } from './common/networkStubs';
import { wrapper } from './common/decorator';
import { APIError } from '../error';

const server = setupServer();

server.use(networkStubs.successWithData);
server.use(networkStubs.metadata);

beforeAll(() => server.listen());
afterAll(() => server.close());

describe("useFKRelationships hooks' postgres test", () => {
  test('useTableFKRelationships fetches data correctly', async () => {
    const { result, waitForValueToChange } = renderHook(
      () =>
        useTableFKRelationships({
          table: { name: 'person', schema: 'public' },
          driver: 'postgres',
          source: 'default',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    const firstResult = result.current.data![0];

    expect(firstResult.table_schema).toEqual('public');
    expect(firstResult.table_name).toEqual('car');
    expect(firstResult.constraint_name).toEqual('car_person_id_fkey');
    expect(firstResult.ref_table).toEqual('person');
  });

  test('useDataSourceFKRelationships fetches data correctly', async () => {
    const schemas = ['public', 'export', 'books_schema', 'authors_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useDataSourceFKRelationships({
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
    expect(firstResult.table_schema).toEqual('books_schema');
    expect(firstResult.table_name).toEqual('books');
    expect(firstResult.constraint_name).toEqual('author');
    expect(firstResult.ref_table).toEqual('authors');
  });
});

describe("useFKRelationships hooks' mssql test", () => {
  test('useTableFKRelationships fetches data correctly', async () => {
    const { result, waitForValueToChange } = renderHook(
      () =>
        useTableFKRelationships({
          table: { name: 'testers', schema: 'dbo' },
          driver: 'mssql',
          source: 'mssql',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    const firstResult = result.current.data![0];

    expect(firstResult.table_schema).toEqual('dbo');
    expect(firstResult.table_name).toEqual('testers');
    expect(firstResult.constraint_name).toEqual('FK_flight');
    expect(firstResult.ref_table).toEqual('flights');
  });

  test('useDataSourceFKRelationships fetches data correctly', async () => {
    const schemas = ['dbo', 'sales_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useDataSourceFKRelationships({
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
    expect(firstResult.table_schema).toEqual('sales_schema');
    expect(firstResult.table_name).toEqual('sales');
    expect(firstResult.constraint_name).toEqual('FK_flights');
    expect(firstResult.ref_table).toEqual('flights');
  });
});

describe("useFKRelationships hooks' error hanlding", () => {
  test('useTableFKRelationships handles not-exists error', async () => {
    server.use(networkStubs.errorUnknownSource);
    const { result, waitForValueToChange } = renderHook(
      () =>
        useTableFKRelationships(
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

  test('useDataSourceFKRelationships handles inconsistent source error', async () => {
    server.use(networkStubs.errorWrongDriver);
    const schemas = ['dbo', 'sales_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useDataSourceFKRelationships(
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
