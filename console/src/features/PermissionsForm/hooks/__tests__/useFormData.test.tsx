import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';
import { handlers } from '../../mocks/handlers.mock';

import { useFormData } from '../dataFetchingHooks';

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

describe("useFormData hooks' postgres test", () => {
  test('useFormData fetches data correctly', async () => {
    const roleName = 'user';

    const { result, waitForValueToChange } = renderHook(
      () =>
        useFormData({
          dataTarget,
          roleName,
          queryType: 'insert',
        }),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.data);

    expect(result.current.data?.table?.table_name).toBe('users');
    expect(result.current.data?.columns).toEqual([
      'id',
      'email',
      'name',
      'type',
      'username',
    ]);
    expect(result.current.data?.supportedQueries).toEqual([
      'insert',
      'select',
      'update',
      'delete',
    ]);
    expect(result.current.data?.roles).toEqual(['user']);
  });
});
