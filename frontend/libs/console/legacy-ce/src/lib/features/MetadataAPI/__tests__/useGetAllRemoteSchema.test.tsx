import { UseQueryResult } from 'react-query';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { wrapper } from '../../../hooks/__tests__/common/decorator';
import { useGetAllRemoteSchemaRelationships } from '../hooks/useMetadataRemoteSchemas';

const export_metadata_mock_response: Record<string, any> = {
  resource_version: 100,
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
              name: 'remote_table',
            },
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
    remote_schemas: [
      {
        name: 'destination_remote_schema',
        definition: {
          url: 'https://graphql-pokemon2.vercel.app/',
          timeout_seconds: 60,
        },
        comment: '',
      },
      {
        name: 'source_remote_schema',
        definition: {
          url: 'https://countries.trevorblades.com/',
          timeout_seconds: 60,
        },
        comment: '',
        remote_relationships: [
          {
            relationships: [
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'default',
                    table: 'remote_table',
                    field_mapping: {
                      code: 'id',
                    },
                  },
                },
                name: 'an_example_rs_to_db_relationship',
              },
              {
                definition: {
                  to_remote_schema: {
                    remote_field: {
                      pokemons: {
                        arguments: {},
                      },
                    },
                    remote_schema: 'destination_remote_schema',
                    lhs_fields: ['code'],
                  },
                },
                name: 'test_rel',
              },
              {
                definition: {
                  to_remote_schema: {
                    remote_field: {
                      pokemons: {
                        arguments: {},
                      },
                    },
                    remote_schema: 'destination_remote_schema',
                    lhs_fields: ['code'],
                  },
                },
                name: 'an_example_rs_to_rs_relationship',
              },
            ],
            type_name: 'Country',
          },
        ],
      },
    ],
  },
};

const server = setupServer(
  rest.post('http://localhost/v1/metadata', (req, res, ctx) => {
    return res(
      // Send a valid HTTP status code
      ctx.status(200),
      // And a response body, if necessary
      ctx.json(export_metadata_mock_response)
    );
  })
);

describe('The useGetAllRemoteRelationship hook', () => {
  beforeAll(() => server.listen());
  afterAll(() => server.close());

  it('should return the collection of RS-to-X rels for all Remote schemas', async () => {
    const { result, waitFor } = renderHook(
      () => useGetAllRemoteSchemaRelationships(),
      {
        wrapper,
      }
    );

    await waitFor(() => result.current.isSuccess);

    expect(
      (result.current as UseQueryResult<Record<string, unknown>, Error>).status
    ).toBe('success');

    // if the data is correct. snapshot testing of the data
    expect(
      (result.current as UseQueryResult<Record<string, unknown>, Error>).data
    ).toMatchInlineSnapshot(`
      [
        {
          "relationships": [
            {
              "definition": {
                "to_source": {
                  "field_mapping": {
                    "code": "id",
                  },
                  "relationship_type": "object",
                  "source": "default",
                  "table": "remote_table",
                },
              },
              "name": "an_example_rs_to_db_relationship",
            },
            {
              "definition": {
                "to_remote_schema": {
                  "lhs_fields": [
                    "code",
                  ],
                  "remote_field": {
                    "pokemons": {
                      "arguments": {},
                    },
                  },
                  "remote_schema": "destination_remote_schema",
                },
              },
              "name": "test_rel",
            },
            {
              "definition": {
                "to_remote_schema": {
                  "lhs_fields": [
                    "code",
                  ],
                  "remote_field": {
                    "pokemons": {
                      "arguments": {},
                    },
                  },
                  "remote_schema": "destination_remote_schema",
                },
              },
              "name": "an_example_rs_to_rs_relationship",
            },
          ],
          "rsName": "source_remote_schema",
          "type_name": "Country",
        },
      ]
    `);
  });
});
