import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';
import { handlers } from '../../mocks/handlers.mock';

import { useBulkDeletePermissions } from '../submitHooks';

const mocks = handlers('http://localhost');

const server = setupServer(...mocks);

beforeAll(() => server.listen());
afterAll(() => server.close());

describe("useBulkDeletePermissions hooks' postgres test", () => {
  test('bulk delete permissions submits correctly', async () => {
    const schemaName = 'public';
    const tableName = 'users';

    const { result, waitFor } = renderHook(
      () =>
        useBulkDeletePermissions({
          schemaName,
          tableName,
        }),
      { wrapper }
    );

    const roles = ['user'];

    await waitFor(() => result.current.isLoading === false);

    await result.current.submit(roles);
    expect(result.current.data).toEqual([{ message: 'success' }]);
  });
});
