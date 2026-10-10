import { renderHook } from '@testing-library/react';
import { useMetadataMigration } from '@hasura/metadata/api';
import { useSetRoleToAllowListPermission } from './useSetRoleToAllowListPermissions';

vi.mock('@hasura/metadata/api', () => ({
  useMetadataMigration: vi.fn(),
}));

const mockUseMetadataMigration = vi.mocked(useMetadataMigration);
const mutate = vi.fn();

describe('useSetRoleToAllowListPermission', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mockUseMetadataMigration.mockReturnValue({
      mutate,
    } as unknown as ReturnType<typeof useMetadataMigration>);
  });

  it('sets a global scope when no roles are provided', () => {
    const { result } = renderHook(() =>
      useSetRoleToAllowListPermission('my_collection'),
    );

    result.current.setRoleToAllowListPermission([]);

    expect(mutate).toHaveBeenCalledTimes(1);
    expect(mutate).toHaveBeenCalledWith(
      {
        query: {
          type: 'update_scope_of_collection_in_allowlist',
          args: {
            collection: 'my_collection',
            scope: { global: true },
          },
        },
      },
      undefined,
    );
  });

  it('restricts the scope to the provided roles', () => {
    const { result } = renderHook(() =>
      useSetRoleToAllowListPermission('my_collection'),
    );

    result.current.setRoleToAllowListPermission(['user', 'editor']);

    expect(mutate).toHaveBeenCalledWith(
      {
        query: {
          type: 'update_scope_of_collection_in_allowlist',
          args: {
            collection: 'my_collection',
            scope: { global: false, roles: ['user', 'editor'] },
          },
        },
      },
      undefined,
    );
  });

  it('forwards mutate options to the migration', () => {
    const options = { onSuccess: vi.fn() };
    const { result } = renderHook(() =>
      useSetRoleToAllowListPermission('my_collection'),
    );

    result.current.setRoleToAllowListPermission(['user'], options);

    expect(mutate).toHaveBeenCalledWith(expect.any(Object), options);
  });
});
