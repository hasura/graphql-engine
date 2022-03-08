import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';
import { handlers } from '../../mocks/handlers.mock';

import { useDefaultValues } from '../dataFetchingHooks';

const [query, metadata] = handlers('http://localhost');

const server = setupServer();

server.use(metadata);
server.use(query);

beforeAll(() => server.listen());
afterAll(() => server.close());

describe("useDefaultValues hooks' postgres test", () => {
  test('useDefaultValues fetches data correctly', async () => {
    const schemaName = 'public';
    const tableName = 'users';
    const roleName = 'user';

    const { result, waitForValueToChange, waitFor } = renderHook(
      () =>
        useDefaultValues({
          schemaName,
          tableName,
          roleName,
          queryType: 'insert',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.data);
    await waitFor(() => result.current.isLoading === false);

    const expectedResult = {
      checkType: 'custom',
      filterType: 'none',
      check: '{"id":{"_eq":1}}',
      filter: '',
      rowCount: '0',
      columns: {
        id: false,
        email: true,
        name: false,
        type: true,
        username: false,
      },
      presets: [],
      backendOnly: false,
      aggregationEnabled: false,
      clonePermissions: [],
      allRowChecks: [{ queryType: 'select', value: '{"id":{"_eq":1}}' }],
    };

    expect(result.current.data!).toEqual(expectedResult);
  });
});
