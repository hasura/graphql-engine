// @vitest-environment jsdom
import { act, renderHook } from '@testing-library/react';
import { usePagination } from './usePagination';

describe('usePagination', () => {
  it('defaults to limit 10, offset 0, and a single default sort', () => {
    const { result } = renderHook(() => usePagination());

    expect(result.current.paginationState).toEqual({
      limit: 10,
      offset: 0,
      sorts: [{ column: 'created_at', type: 'asc', nulls: 'last' }],
    });
  });

  it('merges partial initial state over the defaults', () => {
    const { result } = renderHook(() => usePagination({ limit: 25 }));

    expect(result.current.paginationState).toEqual({
      limit: 25,
      offset: 0,
      sorts: [{ column: 'created_at', type: 'asc', nulls: 'last' }],
    });
  });

  it('respects a fully custom initial state', () => {
    const initialState = {
      limit: 50,
      offset: 100,
      sorts: [{ column: 'id', type: 'desc' as const, nulls: 'first' as const }],
    };

    const { result } = renderHook(() => usePagination(initialState));

    expect(result.current.paginationState).toEqual(initialState);
  });

  it('updates state via setPaginationState', () => {
    const { result } = renderHook(() => usePagination());

    act(() => {
      result.current.setPaginationState({
        limit: 20,
        offset: 20,
        sorts: [],
      });
    });

    expect(result.current.paginationState).toEqual({
      limit: 20,
      offset: 20,
      sorts: [],
    });
  });
});
