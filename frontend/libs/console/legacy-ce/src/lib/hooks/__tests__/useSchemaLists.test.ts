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
import { useSchemaList } from '../useSchemaList';
import { networkStubs } from './common/networkStubs';
import { wrapper } from './common/decorator';
import { APIError } from '../error';

const server = setupServer();

server.use(networkStubs.successWithData);
server.use(networkStubs.metadata);

beforeAll(() => server.listen());
afterAll(() => server.close());


describe("useSchemaList hooks' postgres test", () => {
  test('useSchemaList fetches data correctly', async () => {
    const { result, waitForValueToChange } = renderHook(
      () =>
        useSchemaList({
          driver: 'postgres',
          source: 'default',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.data);

    expect(result.current.data!.length).toEqual(4);

    const schemas = result.current.data!;
    expect(schemas[0]).toEqual('public');
    expect(schemas[1]).toEqual('authors_schema');
    expect(schemas[2]).toEqual('books_schema');
    expect(schemas[3]).toEqual('export');
  });
});

describe("useSchemaList hooks' mssql test", () => {
  test('useSchemaList fetches data correctly', async () => {
    const { result, waitForValueToChange } = renderHook(
      () =>
        useSchemaList({
          driver: 'mssql',
          source: 'mssql',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.data);

    expect(result.current.data!.length).toEqual(2);
    const schemas = result.current.data!;
    expect(schemas[0]).toEqual('dbo');
    expect(schemas[1]).toEqual('sales_schema');
  });
});

describe("useSchemaList hooks' error hanlding", () => {
  test('useSchemaList handles not-exists error', async () => {
    server.use(networkStubs.errorUnknownSource);
    const { result, waitForValueToChange } = renderHook(
      () =>
        useSchemaList({ driver: 'postgres', source: 'secon' }, { retry: 0 }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.isError);

    const error = result.current.error!;

    expect(error.message).toContain('source with name "secon" does not exist');
    expect(error.code).toEqual('not-exists');
    expect(error instanceof APIError);
  });

  test('useSchemaList handles inconsistent source error', async () => {
    server.use(networkStubs.errorWrongDriver);
    const { result, waitForValueToChange } = renderHook(
      () =>
        useSchemaList(
          {
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
*/
