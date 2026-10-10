import { renderHook, act } from '@testing-library/react';
import { useSetTableAsEnum } from './useSetTableAsEnum';

const mutate = vi.fn();

vi.mock('@hasura/metadata/api', () => ({
  useMetadataMigration: () => ({ mutate, isPending: false }),
  useMetadata: () => ({
    data: { source: { kind: 'postgres' }, resource_version: 5 },
  }),
  useErrorNotification: () => vi.fn(),
}));

vi.mock('@hasura/metadata/helpers', () => ({
  getDriverPrefix: () => 'pg',
  MetadataSelectors: { findMetadataSource: () => ({ kind: 'postgres' }) },
}));

vi.mock('@hasura/shared/ui', () => ({ hasuraToast: vi.fn() }));

const table = { name: 'colors', schema: 'public' };

describe('useSetTableAsEnum', () => {
  beforeEach(() => mutate.mockClear());

  it('issues a backend-prefixed set_table_is_enum migration with is_enum=true', () => {
    const { result } = renderHook(() => useSetTableAsEnum('default', table));
    act(() => {
      result.current.setTableAsEnum(true);
    });

    expect(mutate).toHaveBeenCalledTimes(1);
    expect(mutate).toHaveBeenCalledWith(
      {
        query: {
          resource_version: 5,
          type: 'pg_set_table_is_enum',
          args: { source: 'default', table, is_enum: true },
        },
      },
      expect.objectContaining({
        onSuccess: expect.any(Function),
        onError: expect.any(Function),
      }),
    );
  });

  it('supports unsetting enum (is_enum=false) and invokes the caller onSuccess', () => {
    mutate.mockImplementation((_query, opts) => opts?.onSuccess?.());
    const onSuccess = vi.fn();
    const { result } = renderHook(() => useSetTableAsEnum('default', table));

    act(() => {
      result.current.setTableAsEnum(false, { onSuccess });
    });

    expect(mutate).toHaveBeenCalledWith(
      expect.objectContaining({
        query: expect.objectContaining({
          type: 'pg_set_table_is_enum',
          args: expect.objectContaining({ is_enum: false }),
        }),
      }),
      expect.any(Object),
    );
    expect(onSuccess).toHaveBeenCalledTimes(1);
  });
});
