import { renderHook } from '@testing-library/react-hooks';
import { QueryClient, QueryClientProvider, UseQueryResult } from 'react-query';
import React from 'react';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { Provider } from 'react-redux';
import { store } from '../../../store';
import { useRemoteDatabaseRelationships } from '..';
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
  resource_version: 5,
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
              name: 'test',
            },
            remote_relationships: [
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'chinook',
                    table: 'Album',
                    field_mapping: {
                      id: 'AlbumId',
                    },
                  },
                },
                name: 'name_of_the_remote_relationship_1',
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
      {
        name: 'chinook',
        kind: 'postgres',
        tables: [
          {
            table: {
              schema: 'public',
              name: 'Album',
            },
          },
        ],
        configuration: {
          connection_info: {
            use_prepared_statements: false,
            database_url:
              'postgres://postgres:test@host.docker.internal:6001/chinook',
            isolation_level: 'read-committed',
          },
        },
      },
    ],
  },
};

const server = setupServer(
  rest.post('http://localhost/v1/metadata', (req, res, ctx) => {
    return res(ctx.status(200), ctx.json(mockResponse));
  })
);

describe('The useRemoteDatabaseRelationships hook', () => {
  beforeAll(() => server.listen());
  afterAll(() => server.close());

  it('should show loading status whilst fetching data', () => {
    const { result } = renderHook(
      () =>
        useRemoteDatabaseRelationships({
          database: 'default',
          schema: 'public',
          table: 'test',
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
        useRemoteDatabaseRelationships({
          database: 'default',
          schema: 'public',
          table: 'test',
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
            "to_source": {
              "field_mapping": {
                "id": "AlbumId",
              },
              "relationship_type": "object",
              "source": "chinook",
              "table": "Album",
            },
          },
          "name": "name_of_the_remote_relationship_1",
        },
      ]
    `);
  });
});
