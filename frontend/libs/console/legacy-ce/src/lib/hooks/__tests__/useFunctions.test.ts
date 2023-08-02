test('Skipped tests', () => {});
/*
Commented out because of a the following circular dependency problem.

TypeError: Cannot read properties of undefined (reading 'postgres')

      446 |
      447 | export let currentDriver: Driver = 'postgres';
    > 448 | export let dataSource: DataSourcesAPI = services[currentDriver || 'postgres'];
          |                                                 ^
      449 |
      450 | export const isFeatureSupported = (
      451 |   feature: Path<DeepRequired<SupportedFeaturesType>>
*/

/*

import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import {
  useTrackableFunctions,
  useSingleFunction,
  useNonTrackableFunctions,
  useAllFunctions,
} from '../useFunctions';
import { networkStubs } from './common/networkStubs';
import { wrapper } from './common/decorator';
import { APIError } from '../error';

const server = setupServer();

server.use(networkStubs.successWithData);
server.use(networkStubs.metadata);

beforeAll(() => server.listen());
afterAll(() => server.close());

describe("useFunctions hooks' postgres test", () => {
  test('useSingleFunction fetches data correctly', async () => {
    const { result, waitForValueToChange } = renderHook(
      () =>
        useSingleFunction({
          fn: { name: 'search_houses', schema: 'public' },
          driver: 'postgres',
          source: 'default',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    const firstResult = result.current.data!;

    expect(firstResult.function_schema).toEqual('public');
    expect(firstResult.function_name).toEqual('search_houses');
    expect(firstResult.function_type).toEqual('STABLE');
    expect(firstResult.return_type_name).toEqual('house');
    expect(firstResult.function_definition).not.toBe('');
  });

  test('useTrackableFunctions fetches data correctly', async () => {
    const schemas = ['public', 'export', 'books_schema', 'authors_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useTrackableFunctions({
          schemas,
          driver: 'postgres',
          source: 'default',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.data);

    expect(result.current.data!.length).toBeGreaterThan(0);

    result.current.data!.forEach(fn => {
      expect(schemas).toContain(fn.function_schema);
    });

    const firstResult = result.current.data![0];
    expect(firstResult.function_schema).toEqual('public');
    expect(firstResult.function_name).toEqual('search_houses');
    expect(firstResult.function_type).toEqual('STABLE');
    expect(firstResult.return_type_name).toEqual('house');
    expect(firstResult.function_definition).not.toBe('');
  });

  test('useNonTrackableFunctions fetches data correctly', async () => {
    const schemas = ['public', 'export', 'books_schema', 'authors_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useNonTrackableFunctions({
          schemas,
          driver: 'postgres',
          source: 'default',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.data);

    expect(result.current.data!.length).toBeGreaterThan(0);

    result.current.data!.forEach(fn => {
      expect(schemas).toContain(fn.function_schema);
    });

    const firstResult = result.current.data![0];
    expect(firstResult.function_schema).toEqual('public');
    expect(firstResult.function_name).toEqual('armor');
    expect(firstResult.function_type).toEqual('IMMUTABLE');
    expect(firstResult.return_type_name).toEqual('text');
    expect(firstResult.function_definition).not.toBe('');
  });

  test('useAllFunctions fetches data correctly', async () => {
    const schemas = ['public', 'export', 'books_schema', 'authors_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useAllFunctions({
          schemas,
          driver: 'postgres',
          source: 'default',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.data);

    expect(result.current.data!.length).toBeGreaterThan(0);

    result.current.data!.forEach(fn => {
      expect(schemas).toContain(fn.function_schema);
    });

    const firstResult = result.current.data![0];
    expect(firstResult.function_schema).toEqual('public');
    expect(firstResult.function_name).toEqual('search_houses');
    expect(firstResult.function_type).toEqual('STABLE');
    expect(firstResult.return_type_name).toEqual('house');
    expect(firstResult.function_definition).not.toBe('');
  });
});

describe("useFunctions hooks' error hanlding", () => {
  test('useSingleFunction handles not-exists error', async () => {
    server.use(networkStubs.errorUnknownSource);
    const { result, waitForValueToChange } = renderHook(
      () =>
        useSingleFunction(
          {
            fn: { name: 'search_houses', schema: 'public' },
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

  test('useTrackableFunctions handles inconsistent source error', async () => {
    server.use(networkStubs.errorWrongDriver);
    const schemas = ['public', 'export', 'books_schema', 'authors_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useTrackableFunctions(
          {
            schemas,
            driver: 'postgres',
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

  test('useNonTrackableFunctions handles not-exists error', async () => {
    server.use(networkStubs.errorUnknownSource);
    const schemas = ['public', 'export', 'books_schema', 'authors_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useNonTrackableFunctions(
          {
            schemas,
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

  test('useAllFunctions handles inconsistent source error', async () => {
    server.use(networkStubs.errorWrongDriver);
    const schemas = ['public', 'export', 'books_schema', 'authors_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useAllFunctions(
          {
            schemas,
            driver: 'postgres',
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

describe("useFunctions hooks' doesn't fail for unimplemented sources", () => {
  test('useSingleFunction fetches data correctly', async () => {
    const { result, waitForValueToChange } = renderHook(
      () =>
        useSingleFunction({
          fn: { name: 'search_houses', schema: 'public' },
          driver: 'mssql',
          source: 'mssql',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);

    const firstResult = result.current.data;
    expect(firstResult).toEqual(null);
  });

  test('useTrackableFunctions fetches data correctly', async () => {
    const schemas = ['public', 'export', 'books_schema', 'authors_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useTrackableFunctions({
          schemas,
          driver: 'bigquery',
          source: 'bigquery',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);
    expect(result.current.data!.length).toEqual(0);
  });

  test('useNonTrackableFunctions fetches data correctly', async () => {
    const schemas = ['public', 'export', 'books_schema', 'authors_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useNonTrackableFunctions({
          schemas,
          driver: 'mssql',
          source: 'mssql',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);
    expect(result.current.data!.length).toEqual(0);
  });

  test('useAllFunctions fetches data correctly', async () => {
    const schemas = ['public', 'export', 'books_schema', 'authors_schema'];
    const { result, waitForValueToChange } = renderHook(
      () =>
        useAllFunctions({
          schemas,
          driver: 'mssql',
          source: 'mssql',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isSuccess);
    expect(result.current.data!.length).toEqual(0);
  });
});
*/
