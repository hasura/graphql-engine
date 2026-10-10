import { renderHook, waitFor } from '@testing-library/react';
import { setupServer } from 'msw/node';
import { http } from 'msw';
import { handlers, testWrapper } from '@hasura/shared/testing';
import { useUpdateAPILimits, type ApiLimitInput } from './useUpdateAPILimits';
import { useRemoveAPILimits } from './useRemoveAPILimits';

// HTTP-boundary tests: they assert the exact metadata request contract these
// hooks send to `POST /v1/metadata`, mocked with the shared MSW v2 `handlers()`.
// They do NOT talk to a live graphql-engine.
//
// A capture handler is prepended and returns `undefined`, so MSW falls through
// to the shared `handlers()` for the real success response + version bump.
let capturedBodies: any[] = [];

const captureHandler = http.post('*/v1/metadata', async ({ request }) => {
  const body = (await request.clone().json()) as any;
  if (body?.type !== 'export_metadata') {
    capturedBodies.push(body);
  }
  return undefined;
});

const server = setupServer(captureHandler, ...handlers());

describe('useUpdateAPILimits / useRemoveAPILimits (HTTP boundary)', () => {
  beforeAll(() => server.listen());
  beforeEach(() => {
    capturedBodies = [];
    vitest.spyOn(console, 'error').mockImplementation(() => null);
  });
  afterEach(() => {
    vitest.spyOn(console, 'error').mockRestore();
    server.resetHandlers(captureHandler, ...handlers());
  });
  afterAll(() => server.close());

  it('useUpdateAPILimits POSTs a set_api_limits metadata request', async () => {
    const input: ApiLimitInput = {
      newAPILimits: {
        disabled: false,
        depth_limit: { global: 5, state: 'enabled' },
      },
    };

    const { result } = renderHook(() => useUpdateAPILimits(), {
      wrapper: testWrapper,
    });

    await result.current(input);

    await waitFor(() => expect(capturedBodies.length).toBe(1));
    expect(capturedBodies[0]).toEqual({
      type: 'set_api_limits',
      args: { disabled: false, depth_limit: { global: 5 } },
    });
  });

  it('useRemoveAPILimits POSTs a remove_api_limits metadata request for the global role', async () => {
    const { result } = renderHook(() => useRemoveAPILimits(), {
      wrapper: testWrapper,
    });

    await result.current({
      existingAPILimits: { disabled: false },
      role: 'global',
    });

    await waitFor(() => expect(capturedBodies.length).toBe(1));
    expect(capturedBodies[0]).toEqual({
      type: 'remove_api_limits',
      args: { disabled: false },
    });
  });
});
