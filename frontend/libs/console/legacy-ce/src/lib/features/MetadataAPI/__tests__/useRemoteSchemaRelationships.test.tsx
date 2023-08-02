import { renderHook } from '@testing-library/react-hooks';
import { QueryClient, QueryClientProvider, UseQueryResult } from 'react-query';
import React from 'react';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { Provider } from 'react-redux';
import { store } from '../../../store';
import { useRemoteSchemaRelationships } from '..';
import { RemoteRelationship } from '../../../metadata/types';

const queryClient = new QueryClient();
const wrapper = ({ children }: { children: React.ReactNode }) => (
  <Provider store={store}>
    <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  </Provider>
);

export const FETCHING_STATUS = 'loading';
export const FETCHED_STATUS = 'success';

const mockResponse = {
  resource_version: 67,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'default',
        kind: 'postgres',
        tables: [
          {
            table: {
              schema: 'public',
              name: 'person',
            },
            remote_relationships: [
              {
                definition: {
                  to_remote_schema: {
                    remote_schema: 'name_of_the_remote_schema',
                    lhs_fields: ['id'],
                    remote_field: {
                      countries: {
                        arguments: {
                          filter: {
                            code: {
                              eq: '$id',
                            },
                          },
                        },
                      },
                    },
                  },
                },
                name: 'name_of_the_remote_relationship',
              },
            ],
          },
        ],
        configuration: {
          connection_info: {
            use_prepared_statements: true,
            database_url: {
              from_env: 'HASURA_GRAPHQL_DATABASE_URL',
            },
            isolation_level: 'read-committed',
            pool_settings: {
              connection_lifetime: 600,
              retries: 1,
              idle_timeout: 180,
              max_connections: 50,
            },
          },
        },
      },
    ],
  },
};

const server = setupServer(
  rest.post('http://localhost/v1/metadata', (req, res, ctx) => {
    // return res(ctx.json(mockResponse));
    return res(
      // Send a valid HTTP status code
      ctx.status(200),
      // And a response body, if necessary
      ctx.json(mockResponse)
    );
  })
);

describe('The useRemoteSchemaRelationships hook', () => {
  beforeAll(() => server.listen());
  afterAll(() => server.close());

  it('should show loading status whilst fetching data', () => {
    const { result } = renderHook(
      () =>
        useRemoteSchemaRelationships('default', {
          schema: 'public',
          name: 'person',
        }),
      {
        wrapper,
      }
    );
    expect(
      (result.all[0] as UseQueryResult<RemoteRelationship[], Error>).status
    ).toEqual(FETCHING_STATUS);
  });

  it('should show success status after fetching data and data format should be correct', async () => {
    const { result, waitForNextUpdate } = renderHook(
      () =>
        useRemoteSchemaRelationships('default', {
          schema: 'public',
          name: 'person',
        }),
      {
        wrapper,
      }
    );
    await waitForNextUpdate();
    expect(
      (result.all[1] as UseQueryResult<RemoteRelationship[], Error>).status
    ).toEqual(FETCHED_STATUS);

    expect((result.all[1] as UseQueryResult<RemoteRelationship[], Error>).data)
      .toMatchInlineSnapshot(`
      [
        {
          "definition": {
            "to_remote_schema": {
              "lhs_fields": [
                "id",
              ],
              "remote_field": {
                "countries": {
                  "arguments": {
                    "filter": {
                      "code": {
                        "eq": "$id",
                      },
                    },
                  },
                },
              },
              "remote_schema": "name_of_the_remote_schema",
            },
          },
          "name": "name_of_the_remote_relationship",
        },
      ]
    `);
  });
});
