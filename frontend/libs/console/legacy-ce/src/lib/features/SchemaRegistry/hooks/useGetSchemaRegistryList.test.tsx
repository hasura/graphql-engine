import * as React from 'react';
import { renderHook, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { useGetSchemaRegistryList } from './useGetSchemaRegistryList';
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

const v2List = [{ id: 'v2-a' }, { id: 'v2-b' }];
const v1List = [{ id: 'v1-a' }];

const mockQueries = () =>
  vi
    .spyOn(schemaRegsitryControlPlaneClient, 'query')
    .mockImplementation((_query: any, vars: any) => {
      if (vars && 'lastV2EntryCursor' in vars) {
        return Promise.resolve({
          data: {
            schema_registry_dumps_aggregate: { aggregate: { count: 12 } },
          },
        }) as any;
      }
      if (vars && 'changeTimestamp' in vars) {
        return Promise.resolve({
          data: { schema_registry_dumps: v1List },
        }) as any;
      }
      return Promise.resolve({
        data: { schema_registry_dumps_v2: v2List },
      }) as any;
    });

describe('useGetSchemaRegistryList', () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it('returns only v2 dumps when the page sits below the last v2 page', async () => {
    mockQueries();

    // v2Count = 50 -> lastV2Page = 5, pageNumber 0 < 5 -> response is v2 dumps.
    let renderCount = 0;
    const { result } = renderHook(
      () => {
        renderCount++;
        return useGetSchemaRegistryList('p', 0, 50, 'now()');
      },
      { wrapper: makeWrapper() },
    );

    expect(result.current.kind).toEqual('loading');

    await waitFor(() => expect(result.current.kind).toEqual('success'));

    expect(result.current).toEqual({
      kind: 'success',
      response: v2List,
      totalCount: 62, // v2Count (50) + derived v1Count (12)
    });

    // Should settle without a runaway render loop.
    await new Promise((r) => setTimeout(r, 25));
    expect(renderCount).toBeLessThanOrEqual(8);
  });

  it('merges v2 and v1 dumps on the boundary page', async () => {
    mockQueries();

    // v2Count = 10 -> lastV2Page = 1, pageNumber 1 === lastV2Page -> merged.
    const { result } = renderHook(
      () => useGetSchemaRegistryList('p', 1, 10, 'now()'),
      { wrapper: makeWrapper() },
    );

    await waitFor(() => expect(result.current.kind).toEqual('success'));

    expect(result.current).toEqual({
      kind: 'success',
      response: [...v2List, ...v1List],
      totalCount: 22, // v2Count (10) + derived v1Count (12)
    });
  });
});
