import { renderHook } from '@testing-library/react';
import { useMetadata, useMetadataMigration } from '@hasura/metadata/api';
import { useAddToAllowList } from './useAddToAllowList';

vi.mock('@hasura/metadata/api', () => ({
  useMetadata: vi.fn(),
  useMetadataMigration: vi.fn(),
}));

const mockUseMetadata = vi.mocked(useMetadata);
const mockUseMetadataMigration = vi.mocked(useMetadataMigration);

const mutate = vi.fn();

const setupMigration = (
  overrides: Partial<ReturnType<typeof useMetadataMigration>> = {},
) => {
  mockUseMetadataMigration.mockReturnValue({
    mutate,
    isSuccess: false,
    isPending: false,
    error: null,
    ...overrides,
  } as unknown as ReturnType<typeof useMetadataMigration>);
};

const setupMetadata = (data: unknown) => {
  mockUseMetadata.mockReturnValue({
    data,
  } as unknown as ReturnType<typeof useMetadata>);
};

describe('useAddToAllowList', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    setupMetadata({ resource_version: 42 });
    setupMigration();
  });

  it('dispatches an add_collection_to_allowlist migration for the given collection', async () => {
    const { result } = renderHook(() => useAddToAllowList());

    await result.current.addToAllowList('my_collection');

    expect(mutate).toHaveBeenCalledTimes(1);
    expect(mutate).toHaveBeenCalledWith(
      {
        query: {
          resource_version: 42,
          type: 'add_collection_to_allowlist',
          args: { collection: 'my_collection' },
        },
      },
      undefined,
    );
  });

  it('omits resource_version when metadata is not yet available', async () => {
    setupMetadata(undefined);

    const { result } = renderHook(() => useAddToAllowList());

    await result.current.addToAllowList('my_collection');

    expect(mutate).toHaveBeenCalledWith(
      {
        query: {
          type: 'add_collection_to_allowlist',
          args: { collection: 'my_collection' },
        },
      },
      undefined,
    );
    const [payload] = mutate.mock.calls[0];
    expect(payload.query).not.toHaveProperty('resource_version');
  });

  it('forwards mutate options (e.g. onSuccess/onError) to the migration', async () => {
    const options = { onSuccess: vi.fn(), onError: vi.fn() };

    const { result } = renderHook(() => useAddToAllowList());

    await result.current.addToAllowList('my_collection', options);

    expect(mutate).toHaveBeenCalledWith(expect.any(Object), options);
  });

  it('exposes the loading state from the underlying migration (isPending -> isLoading)', () => {
    setupMigration({ isPending: true } as never);

    const { result } = renderHook(() => useAddToAllowList());

    expect(result.current.isLoading).toBe(true);
  });

  it('exposes success and error state from the underlying migration', () => {
    const error = new Error('boom');
    setupMigration({ isSuccess: true, error } as never);

    const { result } = renderHook(() => useAddToAllowList());

    expect(result.current.isSuccess).toBe(true);
    expect(result.current.error).toBe(error);
  });
});
