import { act, renderHook } from '@testing-library/react';
import { useMetadata } from '@hasura/metadata/api';
import { useEnabledRolesFromAllowListState } from './useEnabledRolesFromAllowListState';

vi.mock('@hasura/metadata/api', () => ({
  useMetadata: vi.fn(),
}));

const mockUseMetadata = vi.mocked(useMetadata);

// Minimal metadata exercising the real MetadataSelectors used by the hook:
// - `user` comes from a table select permission
// - `editor` comes from a role-scoped allowlist entry
const metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'default',
        kind: 'postgres',
        tables: [
          {
            table: { name: 'users', schema: 'public' },
            select_permissions: [{ role: 'user', permission: {} }],
          },
        ],
      },
    ],
    allowlist: [
      {
        collection: 'restricted_collection',
        scope: { global: false, roles: ['editor'] },
      },
      {
        collection: 'global_collection',
        scope: { global: true },
      },
    ],
  },
};

const setupMetadata = (data: unknown) => {
  mockUseMetadata.mockReturnValue({
    data,
  } as unknown as ReturnType<typeof useMetadata>);
};

describe('useEnabledRolesFromAllowListState', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    setupMetadata(metadata);
  });

  it('derives all available roles from the metadata', () => {
    const { result } = renderHook(() =>
      useEnabledRolesFromAllowListState('restricted_collection'),
    );

    expect(result.current.allAvailableRoles).toEqual(
      expect.arrayContaining(['user', 'editor']),
    );
  });

  it('returns the roles enabled for a role-scoped collection', () => {
    const { result } = renderHook(() =>
      useEnabledRolesFromAllowListState('restricted_collection'),
    );

    expect(result.current.enabledRoles).toEqual(['editor']);
  });

  it('returns an empty list for a globally-scoped collection', () => {
    const { result } = renderHook(() =>
      useEnabledRolesFromAllowListState('global_collection'),
    );

    expect(result.current.enabledRoles).toEqual([]);
  });

  it('returns an empty list for a collection missing from the allowlist', () => {
    const { result } = renderHook(() =>
      useEnabledRolesFromAllowListState('unknown_collection'),
    );

    expect(result.current.enabledRoles).toEqual([]);
  });

  it('keeps only brand-new roles (not already available) in newRoles', () => {
    const { result } = renderHook(() =>
      useEnabledRolesFromAllowListState('restricted_collection'),
    );

    act(() => {
      result.current.setNewRoles(['user', 'brand_new_role']);
    });

    expect(result.current.newRoles).toEqual(['brand_new_role']);
  });

  it('defaults to empty roles when metadata is not available', () => {
    setupMetadata(undefined);

    const { result } = renderHook(() =>
      useEnabledRolesFromAllowListState('restricted_collection'),
    );

    expect(result.current.allAvailableRoles).toEqual([]);
    expect(result.current.enabledRoles).toEqual([]);
  });
});
