import { renderHook, act } from '@testing-library/react';
import { vi } from 'vitest';
import type { TableColumn } from '@hasura/metadata/data-source';
import type { OrderBy, Table } from '@hasura/shared/types';
import { useInitialWhereAndOrderBy } from './useInitialWhereAndOrderBy';

const { getLSItemMock, setLSItemMock } = vi.hoisted(() => ({
  getLSItemMock: vi.fn(),
  setLSItemMock: vi.fn(),
}));

vi.mock('@hasura/shared/utils', async (importOriginal) => ({
  ...(await importOriginal<Record<string, unknown>>()),
  getLSItem: getLSItemMock,
  setLSItem: setLSItemMock,
}));

const table: Table = { schema: 'public', name: 'orders' } as unknown as Table;
const columns = [{ name: 'id' }] as unknown as TableColumn[];

const stored = {
  where: [{ id: { _eq: '1' } }],
  order_by: [{ column: 'id', type: 'asc' }] satisfies OrderBy[],
};

describe('useInitialWhereAndOrderBy', () => {
  beforeEach(() => {
    getLSItemMock.mockReset();
    setLSItemMock.mockReset();
  });

  it('returns undefined options while columns are not yet loaded', () => {
    getLSItemMock.mockReturnValue(JSON.stringify(stored));

    const { result } = renderHook(
      ({ cols }) =>
        useInitialWhereAndOrderBy({
          columns: cols,
          table,
          dataSourceName: 'default',
        }),
      { initialProps: { cols: undefined as TableColumn[] | undefined } },
    );

    expect(result.current.options).toBeUndefined();
  });

  it('derives options from local storage once columns load', () => {
    getLSItemMock.mockReturnValue(JSON.stringify(stored));

    const { result, rerender } = renderHook(
      ({ cols }) =>
        useInitialWhereAndOrderBy({
          columns: cols,
          table,
          dataSourceName: 'default',
        }),
      { initialProps: { cols: undefined as TableColumn[] | undefined } },
    );

    expect(result.current.options).toBeUndefined();

    rerender({ cols: columns });

    expect(result.current.options).toEqual({
      where: stored.where,
      order_by: stored.order_by,
    });
  });

  it('keeps the derived options stable across re-renders (no runaway updates)', () => {
    getLSItemMock.mockReturnValue(JSON.stringify(stored));

    const { result, rerender } = renderHook(() =>
      useInitialWhereAndOrderBy({
        columns,
        table,
        dataSourceName: 'default',
      }),
    );

    const first = result.current.options;
    rerender();
    rerender();
    expect(result.current.options).toBe(first);
  });

  it('persists where/order_by to local storage via onUpdateOptions', () => {
    getLSItemMock.mockReturnValue(undefined);

    const { result } = renderHook(() =>
      useInitialWhereAndOrderBy({
        columns,
        table,
        dataSourceName: 'default',
      }),
    );

    act(() => {
      result.current.onUpdateOptions({
        where: stored.where,
        order_by: stored.order_by,
      });
    });

    expect(setLSItemMock).toHaveBeenCalledWith(
      expect.any(String),
      JSON.stringify({ where: stored.where, order_by: stored.order_by }),
    );
  });
});
