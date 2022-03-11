import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';
import { handlers } from '../../mocks/handlers.mock';

import { useUpdatePermissions } from '../submitHooks';

const mocks = handlers('http://localhost');

const server = setupServer(...mocks);

beforeAll(() => server.listen());
afterAll(() => server.close());

beforeEach(() => {
  jest.spyOn(console, 'error').mockImplementation(() => null);
});

afterEach(() => {
  jest.spyOn(console, 'error').mockRestore();
});

describe("useUpdatePermissions hooks' postgres test", () => {
  test('update permissions submits correctly', async () => {
    const schemaName = 'public';
    const tableName = 'users';
    const roleName = 'user';

    const { result, waitFor } = renderHook(
      () =>
        useUpdatePermissions({
          schemaName,
          tableName,
          roleName,
          queryType: 'insert',
          accessType: 'partialAccess',
        }),
      { wrapper }
    );

    await waitFor(() => result.current.updatePermissions.isLoading === false);

    const formData = {
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

    await result.current.updatePermissions.submit(formData);
    expect(result.current.updatePermissions.data).toEqual([
      { message: 'success' },
    ]);
  });

  test('delete permissions submits correctly', async () => {
    const schemaName = 'public';
    const tableName = 'users';
    const roleName = 'user';

    const { result, waitFor } = renderHook(
      () =>
        useUpdatePermissions({
          schemaName,
          tableName,
          roleName,
          queryType: 'insert',
          accessType: 'partialAccess',
        }),
      { wrapper }
    );
    await waitFor(() => result.current.updatePermissions.isLoading === false);

    await result.current.deletePermissions.submit(['insert']);
    expect(result.current.deletePermissions.data).toEqual([
      { message: 'success' },
    ]);
  });
});
