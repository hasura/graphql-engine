import * as React from 'react';
import { renderHook, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { useGetV2Info } from './useGetV2Info';
import { schemaRegsitryControlPlaneClient } from '../utils';

type Props = { children?: React.ReactNode };

const makeWrapper = () => {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });
  return ({ children }: Props) => (
    <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  );
};

describe('useGetV2Info', () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it('starts in a loading state', () => {
    vi.spyOn(schemaRegsitryControlPlaneClient, 'query').mockReturnValue(
      new Promise(() => {
        /* never resolves -> stays loading */
      }) as any,
    );

    const { result } = renderHook(() => useGetV2Info('project-1'), {
      wrapper: makeWrapper(),
    });

    expect(result.current.kind).toEqual('loading');
  });

  it('derives v2Count and v2Cursor from the fetched aggregate data (no runaway re-renders)', async () => {
    vi.spyOn(schemaRegsitryControlPlaneClient, 'query').mockResolvedValue({
      data: {
        schema_registry_dumps_v2_aggregate: { aggregate: { count: 25 } },
        schema_registry_dumps_v2: [
          { change_recorded_at: '2020-01-01T00:00:00Z' },
        ],
      },
    } as any);

    let renderCount = 0;
    const { result } = renderHook(
      () => {
        renderCount++;
        return useGetV2Info('project-1');
      },
      { wrapper: makeWrapper() },
    );

    await waitFor(() => expect(result.current.kind).toEqual('success'));

    expect(result.current).toEqual({
      kind: 'success',
      v2Count: 25,
      v2Cursor: '2020-01-01T00:00:00Z',
    });

    // give any stray effects a chance to fire and re-render, then assert the
    // loading -> success transition settled in a small, bounded number of
    // renders (no runaway re-render loop).
    await new Promise((r) => setTimeout(r, 20));
    expect(renderCount).toBeLessThanOrEqual(4);
  });

  it('falls back to defaults when the aggregate data is empty', async () => {
    vi.spyOn(schemaRegsitryControlPlaneClient, 'query').mockResolvedValue({
      data: {
        schema_registry_dumps_v2_aggregate: { aggregate: { count: 0 } },
        schema_registry_dumps_v2: [],
      },
    } as any);

    const { result } = renderHook(() => useGetV2Info('project-1'), {
      wrapper: makeWrapper(),
    });

    await waitFor(() => expect(result.current.kind).toEqual('success'));

    expect(result.current).toEqual({
      kind: 'success',
      v2Count: 0,
      v2Cursor: 'now()',
    });
  });
});
