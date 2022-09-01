import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';
import { handlers } from '../../mocks/handlers.mock';

import { useBulkDeletePermissions } from '../submitHooks';

const mocks = handlers('http://localhost');

const server = setupServer(...mocks);

beforeAll(() => server.listen());
afterAll(() => server.close());

const dataLeaf = {
  type: 'schema',
  name: 'users',
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

describe("useBulkDeletePermissions hooks' postgres test", () => {
  test('bulk delete permissions submits correctly', async () => {
    const { result, waitFor } = renderHook(
      () => useBulkDeletePermissions(dataTarget),
      { wrapper }
    );

    const roles = ['user'];

    await waitFor(() => result.current.isLoading === false);

    await result.current.submit(roles);
    expect(result.current.data).toEqual([{ message: 'success' }]);
  });
});
