import { act, cleanup, renderHook } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { AppContext, defaultAppState } from '@hasura/shared/context';
import { HttpError } from '@hasura/shared/types';
import { requestJson } from '@hasura/shared/utils';
import { INCONSISTENT_METADATA_QUERY_KEY } from '../constants';
import { useFetchInconsistentMetadata } from './useInconsistentMetadata';

vi.mock('@hasura/shared/utils', async (importOriginal) => ({
  ...(await importOriginal<typeof import('@hasura/shared/utils')>()),
  requestJson: vi.fn(),
}));

const success = { is_consistent: true, inconsistent_objects: [] };

afterEach(() => {
  cleanup();
  vi.resetAllMocks();
});

const mount = () => {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });
  const hook = renderHook(() => useFetchInconsistentMetadata(), {
    wrapper: ({ children }) => (
      <QueryClientProvider client={queryClient}>
        <AppContext.Provider
          value={{
            ...defaultAppState,
            endpoints: {
              ...defaultAppState.endpoints,
              metadata: '/v1/metadata',
            },
          }}
        >
          {children}
        </AppContext.Provider>
      </QueryClientProvider>
    ),
  });
  return { ...hook, queryClient };
};

describe('useFetchInconsistentMetadata credential validation', () => {
  it('rejects invalid credentials even when metadata has a fresh cached success', async () => {
    const { result, queryClient } = mount();
    queryClient.setQueryData([INCONSISTENT_METADATA_QUERY_KEY], success);
    const error = new HttpError(400, 'invalid token', { code: 'invalid-jwt' });
    vi.mocked(requestJson).mockRejectedValueOnce(error);
    await act(async () => {
      await expect(
        result.current({ Authorization: 'Bearer invalid' }),
      ).rejects.toBe(error);
    });
    expect(requestJson).toHaveBeenCalledExactlyOnceWith('/v1/metadata', {
      method: 'POST',
      headers: { Authorization: 'Bearer invalid' },
      body: JSON.stringify({ type: 'get_inconsistent_metadata', args: {} }),
    });
  });

  it('revalidates even the same credentials on every call', async () => {
    const { result } = mount();
    const headers = { 'x-hasura-admin-secret': 'secret' };
    vi.mocked(requestJson)
      .mockResolvedValueOnce(success)
      .mockRejectedValueOnce(new HttpError(401, 'secret rotated'));
    await expect(result.current(headers)).resolves.toEqual(success);
    await expect(result.current(headers)).rejects.toBeInstanceOf(HttpError);
    expect(requestJson).toHaveBeenCalledTimes(2);
  });

  it('does not deduplicate concurrent requests using different credentials', async () => {
    const { result } = mount();
    let resolveFirst!: (value: typeof success) => void;
    vi.mocked(requestJson)
      .mockImplementationOnce(
        () =>
          new Promise((resolve) => {
            resolveFirst = resolve;
          }),
      )
      .mockRejectedValueOnce(new HttpError(401, 'bad secret'));
    const first = result.current({ 'x-hasura-admin-secret': 'valid' });
    const second = result.current({ 'x-hasura-admin-secret': 'invalid' });
    await expect(second).rejects.toBeInstanceOf(HttpError);
    resolveFirst(success);
    await expect(first).resolves.toEqual(success);
    expect(requestJson).toHaveBeenCalledTimes(2);
    expect(
      vi.mocked(requestJson).mock.calls.map(([, options]) => options?.headers),
    ).toEqual([
      { 'x-hasura-admin-secret': 'valid' },
      { 'x-hasura-admin-secret': 'invalid' },
    ]);
  });
});
