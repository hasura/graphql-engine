import { renderHook, waitFor } from '@testing-library/react';
import { http, HttpResponse } from 'msw';
import { setupServer } from 'msw/node';
import { vi } from 'vitest';
import { useRemoveAgent } from '../hooks';
import { testWrapper } from '@hasura/shared/testing';

const server = setupServer(
  http.post('http://localhost/v1/metadata', async ({ request }) => {
    const body = (await request.json()) as Record<string, any>;
    if (body.args.name === 'wrong_payload')
      return HttpResponse.json({ message: 'Bad request' }, { status: 400 });
    return HttpResponse.json({ message: 'success' }, { status: 200 });
  }),
);

describe('useRemoveAgent tests: ', () => {
  beforeAll(() => {
    server.listen();
    vi.spyOn(console, 'error').mockImplementation(() => null);
  });
  afterAll(() => {
    server.close();
    vi.spyOn(console, 'error').mockRestore();
  });

  it('calls the custom success callback after adding a DC agent', async () => {
    const { result } = renderHook(() => useRemoveAgent(), {
      wrapper: testWrapper,
    });

    const { removeAgent } = result.current;

    const mockCallback = vi.fn(() => {
      console.log('success');
    });

    removeAgent({
      name: 'test_dc_agent',
      onSuccess: () => {
        mockCallback();
      },
    });

    await waitFor(() => expect(result.current.isSuccess).toBeTruthy());

    await waitFor(() => {
      expect(mockCallback).toHaveBeenCalledTimes(1);
    });
  });

  it('calls the custom error callback after failing to add a DC agent', async () => {
    const { result } = renderHook(() => useRemoveAgent(), {
      wrapper: testWrapper,
    });

    const { removeAgent } = result.current;

    const mockCallback = vi.fn(() => {
      console.log('error');
    });

    removeAgent({
      name: 'wrong_payload',
      onError: () => {
        mockCallback();
      },
    });

    await waitFor(() => expect(result.current.isError).toBeTruthy());

    await waitFor(() => {
      expect(mockCallback).toHaveBeenCalledTimes(1);
    });
  });
});
