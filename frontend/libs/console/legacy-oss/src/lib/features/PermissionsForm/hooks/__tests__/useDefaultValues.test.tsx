import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';
import { handlers } from '../../mocks/handlers.mock';

import { useDefaultValues } from '../dataFetchingHooks';

const [query, metadata] = handlers('http://localhost');

const server = setupServer();

server.use(metadata);
server.use(query);

const dataLeaf = {
  type: 'schema',
  name: 'public',
  leaf: {
    type: 'table',
    name: 'users',
  },
};

const dataTarget = {
  dataSource: {
    driver: 'postgres' as const,
    database: 'default',
  },
  dataLeaf,
};

beforeAll(() => server.listen());
afterAll(() => server.close());

describe("useDefaultValues hooks' postgres test", () => {
  test('useDefaultValues fetches data correctly', async () => {
    const roleName = 'user';

    const { result, waitForValueToChange, waitFor } = renderHook(
      () =>
        useDefaultValues({
          dataTarget,
          roleName,
          queryType: 'insert',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.data);
    await waitFor(() => result.current.isLoading === false);

    expect(result.current.data!).toMatchInlineSnapshot(`
      Object {
        "aggregationEnabled": false,
        "allRowChecks": Array [
          Object {
            "queryType": "select",
            "value": "{\\"id\\":{\\"_eq\\":1}}",
          },
        ],
        "backendOnly": false,
        "check": "{\\"id\\":{\\"_eq\\":1}}",
        "checkType": "custom",
        "clonePermissions": Array [],
        "columns": Object {
          "email": true,
          "id": false,
          "name": false,
          "type": true,
          "username": false,
        },
        "filter": "",
        "filterType": "none",
        "presets": Array [],
        "rowCount": "0",
      }
    `);
  });
});
