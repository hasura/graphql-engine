import { setupServer } from 'msw/node';
import { rest } from 'msw';
import { renderHook } from '@testing-library/react-hooks';
import { metadata, metadata_with_no_query_collections } from './mock/metadata';
import { wrapper } from '../../../../hooks/__tests__/common/decorator';
import { useEnabledRolesFromAllowList } from '../../hooks/AllowListPermissions/useEnabledRolesFromAllowList';

const server = setupServer();

beforeAll(() => server.listen());
afterAll(() => server.close());

describe('useEnabledRolesFromAllowList with valid data', () => {
  beforeEach(() => {
    server.use(
      rest.post('/v1/metadata', (req, res, ctx) =>
        res(ctx.status(200), ctx.json(metadata))
      )
    );
  });
  test('When useEnabledRolesFromAllowList hook is called with query collection Name, then a valid list of enabled roles are returned', async () => {
    const { result, waitForValueToChange } = renderHook(
      () => useEnabledRolesFromAllowList('allowed-queries'),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.data);

    const roles = result.current.data!;

    expect(roles).toEqual(['manager']);
  });
});

describe("useEnabledRolesFromAllowList hooks' with no query collections", () => {
  beforeEach(() => {
    server.use(
      rest.post('/v1/metadata', (req, res, ctx) =>
        res(ctx.status(200), ctx.json(metadata_with_no_query_collections))
      )
    );
  });
  test('When useEnabledRolesFromAllowListn  is called with an invalid query collection, then empty array should be returned', async () => {
    const { result, waitForValueToChange } = renderHook(
      () => useEnabledRolesFromAllowList('allowed-queries'),
      { wrapper }
    );

    await waitForValueToChange(() => result.current.data);

    const roles = result.current.data!;

    expect(roles).toEqual([]);
  });
});
