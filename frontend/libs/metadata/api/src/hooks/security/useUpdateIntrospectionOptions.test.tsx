import { renderHook, waitFor } from '@testing-library/react';
import { setupServer } from 'msw/node';
import { http } from 'msw';
import { handlers, testWrapper } from '@hasura/shared/testing';
import { useUpdateIntrospectionOptions } from './useUpdateIntrospectionOptions';

// HTTP-boundary test: asserts the metadata request contract sent to
// `POST /v1/metadata`, mocked with the shared MSW v2 `handlers()`. Not a
// live graphql-engine integration test.
let capturedBodies: any[] = [];

const captureHandler = http.post('*/v1/metadata', async ({ request }) => {
  const body = (await request.clone().json()) as any;
  if (body?.type !== 'export_metadata') {
    capturedBodies.push(body);
  }
  return undefined;
});

const server = setupServer(captureHandler, ...handlers());

describe('useUpdateIntrospectionOptions (HTTP boundary)', () => {
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

  it('POSTs a set_graphql_schema_introspection_options request adding the role when disabling', async () => {
    const { result } = renderHook(() => useUpdateIntrospectionOptions(), {
      wrapper: testWrapper,
    });

    await result.current({
      existingOptions: [],
      roleName: 'user',
      introspectionIsDisabled: true,
    });

    await waitFor(() => expect(capturedBodies.length).toBe(1));
    expect(capturedBodies[0]).toEqual({
      type: 'set_graphql_schema_introspection_options',
      args: { disabled_for_roles: ['user'] },
    });
  });
});
