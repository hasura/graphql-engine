import { UseQueryResult } from 'react-query';
import { rest } from 'msw';
import { setupServer } from 'msw/node';
import { renderHook } from '@testing-library/react-hooks';
import { wrapper } from '../../../hooks/__tests__/common/decorator';
import { useGetAllRemoteSchemaRelationships } from '../hooks/useGetAllRemoteSchemaRelationships';

let export_metadata_response_type = 'legacy_rs_relationships';

const export_metadata_mock_response: Record<string, any> = {
  legacy_rs_relationships: {
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
                name: 'table1',
              },
              remote_relationships: [
                {
                  definition: {
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
                    hasura_fields: ['id'],
                    remote_schema: 'Remote',
                  },
                  name: 'RSrelationship',
                },
              ],
            },
            {
              table: {
                schema: 'public',
                name: 'table2',
              },
              remote_relationships: [
                {
                  definition: {
                    remote_field: {
                      continents: {
                        arguments: {
                          filter: {
                            code: {
                              eq: '$id',
                            },
                          },
                        },
                      },
                    },
                    hasura_fields: ['id'],
                    remote_schema: 'Remote',
                  },
                  name: 'rel2',
                },
              ],
            },
          ],
          configuration: {},
        },
      ],
      remote_schemas: [
        {
          name: 'Remote',
          definition: {
            url: 'https://countries.trevorblades.com/',
            timeout_seconds: 60,
          },
          comment: '',
        },
      ],
    },
  },
  new_rs_relationships: {
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
                name: 'table1',
              },
              remote_relationships: [
                {
                  definition: {
                    to_remote_schema: {
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
                      hasura_fields: ['id'],
                      remote_schema: 'Remote',
                    },
                  },
                  name: 'RSrelationship',
                },
              ],
            },
          ],
          configuration: {},
        },
      ],
      remote_schemas: [
        {
          name: 'Remote',
          definition: {
            url: 'https://countries.trevorblades.com/',
            timeout_seconds: 60,
          },
          comment: '',
        },
      ],
    },
  },
  legacy_and_new_rs_relationships: {
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
                name: 'table1',
              },
              remote_relationships: [
                {
                  definition: {
                    to_remote_schema: {
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
                      hasura_fields: ['id'],
                      remote_schema: 'Remote',
                    },
                  },
                  name: 'RSrelationship',
                },
              ],
            },
            {
              table: {
                schema: 'public',
                name: 'table2',
              },
              remote_relationships: [
                {
                  definition: {
                    remote_field: {
                      continents: {
                        arguments: {
                          filter: {
                            code: {
                              eq: '$id',
                            },
                          },
                        },
                      },
                    },
                    hasura_fields: ['id'],
                    remote_schema: 'Remote',
                  },
                  name: 'rel2',
                },
              ],
            },
          ],
          configuration: {},
        },
      ],
      remote_schemas: [
        {
          name: 'Remote',
          definition: {
            url: 'https://countries.trevorblades.com/',
            timeout_seconds: 60,
          },
          comment: '',
        },
      ],
    },
  },
};

const server = setupServer(
  rest.post('http://localhost/v1/metadata', (req, res, ctx) => {
    return res(
      // Send a valid HTTP status code
      ctx.status(200),
      // And a response body, if necessary
      ctx.json(export_metadata_mock_response[export_metadata_response_type])
    );
  })
);

describe('The useGetAllRemoteRelationship hook', () => {
  beforeAll(() => server.listen());
  afterAll(() => server.close());

  it('should work when metadata contains legacy rs relationships', async () => {
    export_metadata_response_type = 'legacy_rs_relationships';

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
      Array [
        Object {
          "definition": Object {
            "hasura_fields": Array [
              "id",
            ],
            "remote_field": Object {
              "countries": Object {
                "arguments": Object {
                  "filter": Object {
                    "code": Object {
                      "eq": "$id",
                    },
                  },
                },
              },
            },
            "remote_schema": "Remote",
          },
          "name": "RSrelationship",
          "table_name": "table1",
        },
        Object {
          "definition": Object {
            "hasura_fields": Array [
              "id",
            ],
            "remote_field": Object {
              "continents": Object {
                "arguments": Object {
                  "filter": Object {
                    "code": Object {
                      "eq": "$id",
                    },
                  },
                },
              },
            },
            "remote_schema": "Remote",
          },
          "name": "rel2",
          "table_name": "table2",
        },
      ]
    `);
  });

  it('should work when metadata contains new rs relationships', async () => {
    export_metadata_response_type = 'new_rs_relationships';

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
      Array [
        Object {
          "definition": Object {
            "to_remote_schema": Object {
              "hasura_fields": Array [
                "id",
              ],
              "remote_field": Object {
                "countries": Object {
                  "arguments": Object {
                    "filter": Object {
                      "code": Object {
                        "eq": "$id",
                      },
                    },
                  },
                },
              },
              "remote_schema": "Remote",
            },
          },
          "name": "RSrelationship",
          "table_name": "table1",
        },
      ]
    `);
  });

  it('should work when metadata contains both legacy & new rs relationships', async () => {
    export_metadata_response_type = 'legacy_and_new_rs_relationships';

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
      Array [
        Object {
          "definition": Object {
            "to_remote_schema": Object {
              "hasura_fields": Array [
                "id",
              ],
              "remote_field": Object {
                "countries": Object {
                  "arguments": Object {
                    "filter": Object {
                      "code": Object {
                        "eq": "$id",
                      },
                    },
                  },
                },
              },
              "remote_schema": "Remote",
            },
          },
          "name": "RSrelationship",
          "table_name": "table1",
        },
        Object {
          "definition": Object {
            "hasura_fields": Array [
              "id",
            ],
            "remote_field": Object {
              "continents": Object {
                "arguments": Object {
                  "filter": Object {
                    "code": Object {
                      "eq": "$id",
                    },
                  },
                },
              },
            },
            "remote_schema": "Remote",
          },
          "name": "rel2",
          "table_name": "table2",
        },
      ]
    `);
  });
});
