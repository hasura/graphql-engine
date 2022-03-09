import React from 'react';
import { ComponentMeta, ComponentStory } from '@storybook/react';
import RelationshipsTable from './RelationshipsTable';
import { TableEntry } from '../../metadata/types';

const tableWithLocalRelations: TableEntry = {
  table: { schema: '_helloworld', name: 'artist' },
  object_relationships: [
    {
      name: 'author',
      using: { foreign_key_constraint_on: 'author_id' },
    },
  ],
  array_relationships: [
    {
      name: 'articles',
      using: {
        foreign_key_constraint_on: {
          column: 'author_id',
          table: { schema: '_helloworld', name: 'article' },
        },
      },
    },
  ],
};
const tableWithLegacyRemoteSchemaRelations: any = {
  table: { schema: '_helloworld', name: 'artist' },
  remote_relationships: [
    {
      definition: {
        remote_field: {
          country: {
            field: {
              continent: {
                field: {
                  countries: {
                    field: {
                      states: {
                        arguments: {},
                      },
                    },
                    arguments: {},
                  },
                },
                arguments: {},
              },
            },
            arguments: {},
          },
        },
        hasura_fields: [],
        remote_schema: 'countries',
      },
      name: 'states',
    },
  ],
};

const tableWithRemoteRelations: any = {
  // TODO new metadata type definition is not part of types yet
  // to_source or to_remote_schema is not supported yet
  table: { schema: '_helloworld', name: 'artist' },
  remote_relationships: [
    {
      definition: {
        to_remote_schema: {
          remote_schema: 'countries_rs',
          remote_field: { continents: { arguments: {} } },
        },
      },
      name: 'continents',
    },
    {
      definition: {
        to_source: {
          relationship_type: 'object',
          source: 'chinook',
          table: 'Album',
          field_mapping: {
            artistId: 'id',
          },
        },
      },
      name: 'name_of_the_remote_relationship_1',
    },
  ],
};
const tableWithMixedRelations: any = {
  // TODO new metadata type definition is not part of types yet
  // to_source or to_remote_schema is not supported yet
  table: { schema: '_helloworld', name: 'artist' },
  object_relationships: [
    {
      name: 'author',
      using: { foreign_key_constraint_on: 'author_id' },
    },
  ],
  remote_relationships: [
    {
      definition: {
        remote_field: {
          country: {
            field: {
              continent: {
                field: {
                  countries: {
                    field: {
                      states: {
                        arguments: {},
                      },
                    },
                    arguments: {},
                  },
                },
                arguments: {},
              },
            },
            arguments: {},
          },
        },
        hasura_fields: [],
        remote_schema: 'countries',
      },
      name: 'states',
    },
    {
      definition: {
        to_remote_schema: {
          remote_schema: 'countries_rs',
          remote_field: { continents: { arguments: {} } },
        },
      },
      name: 'continents',
    },
    {
      definition: {
        to_source: {
          relationship_type: 'object',
          source: 'chinook',
          table: 'Album',
          field_mapping: {
            artistId: 'id',
          },
        },
      },
      name: 'name_of_the_remote_relationship_1',
    },
  ],
  array_relationships: [
    {
      name: 'articles',
      using: {
        foreign_key_constraint_on: {
          column: 'author_id',
          table: { schema: '_helloworld', name: 'article' },
        },
      },
    },
  ],
};

export default {
  title: 'Relationships/RelationshipsTable',
  component: RelationshipsTable,
  argTypes: {
    onClick: { action: 'clicked' },
    onDelete: { action: 'clicked' },
    onEdit: { action: 'clicked' },
  },
} as ComponentMeta<typeof RelationshipsTable>;

export const WithOnlyLocalRelationships: ComponentStory<
  typeof RelationshipsTable
> = args => <RelationshipsTable {...args} />;

WithOnlyLocalRelationships.args = {
  tableDefinition: tableWithLocalRelations,
};
export const WithOnlyLegacyRemoteSchemaRelationships: ComponentStory<
  typeof RelationshipsTable
> = args => <RelationshipsTable {...args} />;

WithOnlyLegacyRemoteSchemaRelationships.args = {
  tableDefinition: tableWithLegacyRemoteSchemaRelations,
};

export const WithOnlyRemoteRelationships: ComponentStory<
  typeof RelationshipsTable
> = args => <RelationshipsTable {...args} />;

WithOnlyRemoteRelationships.args = {
  tableDefinition: tableWithRemoteRelations,
};

export const WithAllRelationships: ComponentStory<
  typeof RelationshipsTable
> = args => <RelationshipsTable {...args} />;

WithAllRelationships.args = {
  tableDefinition: tableWithMixedRelations,
};

const customMetadata = {
  resource_version: 51,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'default',
        kind: 'postgres',
        tables: [
          {
            table: {
              schema: '_helloworld',
              name: 'article',
            },
            object_relationships: [
              {
                name: 'author',
                using: {
                  foreign_key_constraint_on: 'author_id',
                },
              },
            ],
            remote_relationships: [
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'bq_test',
                    table: {
                      dataset: 'dataset1',
                      name: 'bq_test_table',
                    },
                    field_mapping: {
                      id: 'PassengerId',
                    },
                  },
                },
                name: 'bq',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'pg',
                    table: {
                      schema: 'public',
                      name: 'test',
                    },
                    field_mapping: {
                      id: 'id',
                    },
                  },
                },
                name: 'citus',
              },
              {
                definition: {
                  remote_field: {
                    continents: {
                      arguments: {},
                    },
                  },
                  hasura_fields: [],
                  remote_schema: 'countries',
                },
                name: 'countries',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'bq_test',
                    table: {
                      dataset: 'dataset1',
                      name: 'bq_test_table',
                    },
                    field_mapping: {
                      id: 'PassengerId',
                    },
                  },
                },
                name: 'sasa',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'bq_test',
                    table: {
                      dataset: 'dataset1',
                      name: 'bq_test_table',
                    },
                    field_mapping: {
                      id: 'PassengerId',
                    },
                  },
                },
                name: 'ss',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'bq_test',
                    table: {
                      dataset: 'dataset1',
                      name: 'bq_test_table',
                    },
                    field_mapping: {
                      id: 'PassengerId',
                    },
                  },
                },
                name: 'ssss',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'pg',
                    table: {
                      schema: 'public',
                      name: 'test',
                    },
                    field_mapping: {
                      id: 'id',
                    },
                  },
                },
                name: 'test',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'array',
                    source: 'citus',
                    table: {
                      schema: 'public',
                      name: 'citus_test',
                    },
                    field_mapping: {
                      id: 'id',
                    },
                  },
                },
                name: 'test_b_array',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'bq_test',
                    table: {
                      dataset: 'dataset1',
                      name: 'bq_test_table',
                    },
                    field_mapping: {
                      id: 'PassengerId',
                    },
                  },
                },
                name: 'testst',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'bq_test',
                    table: {
                      dataset: 'dataset1',
                      name: 'bq_test_table',
                    },
                    field_mapping: {
                      id: 'PassengerId',
                    },
                  },
                },
                name: 'zzzz',
              },
            ],
          },
          {
            table: {
              schema: '_helloworld',
              name: 'author',
            },
            array_relationships: [
              {
                name: 'articles',
                using: {
                  foreign_key_constraint_on: {
                    column: 'author_id',
                    table: {
                      schema: '_helloworld',
                      name: 'article',
                    },
                  },
                },
              },
            ],
          },
          {
            table: {
              schema: 'public',
              name: 'TTTT',
            },
            remote_relationships: [
              {
                definition: {
                  to_source: {
                    relationship_type: 'array',
                    source: 'bq_test',
                    table: {
                      dataset: 'dataset1',
                      name: 'bq_test_table',
                    },
                    field_mapping: {
                      id: 'PassengerId',
                    },
                  },
                },
                name: 'RRR',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'bq_test',
                    table: {
                      dataset: 'dataset1',
                      name: 'bq_test_table',
                    },
                    field_mapping: {
                      id: 'PassengerId',
                    },
                  },
                },
                name: 'Tests',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'bq_test',
                    table: {
                      dataset: 'dataset1',
                      name: 'bq_test_table',
                    },
                    field_mapping: {
                      id: 'PassengerId',
                    },
                  },
                },
                name: 'aaaa',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'citus',
                    table: {
                      schema: 'public',
                      name: 'citus_test',
                    },
                    field_mapping: {
                      id: 'id',
                    },
                  },
                },
                name: 'gggg',
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
        name: 'bq_test',
        kind: 'bigquery',
        tables: [
          {
            table: {
              dataset: 'dataset1',
              name: 'bq_test_table',
            },
            object_relationships: [
              {
                name: 'tttt',
                using: {
                  manual_configuration: {
                    remote_table: {
                      dataset: 'dataset1',
                      name: 'bq_test_table',
                    },
                    insertion_order: null,
                    column_mapping: {
                      PassengerId: 'PassengerId',
                    },
                  },
                },
              },
            ],
            remote_relationships: [
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'citus',
                    table: {
                      schema: 'public',
                      name: 'citus_test',
                    },
                    field_mapping: {
                      PassengerId: 'id',
                    },
                  },
                },
                name: 'r_citus',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'default',
                    table: {
                      schema: 'public',
                      name: 'TTTT',
                    },
                    field_mapping: {
                      PassengerId: 'id',
                    },
                  },
                },
                name: 'rr',
              },
            ],
          },
        ],
        configuration: {
          service_account: {
            project_id: 'hackanoodle',
            client_email: '***@hackanoodle.iam.gserviceaccount.com',
            private_key:
              '-----BEGIN PRIVATE KEY-----\n-----END PRIVATE KEY-----\n',
          },
          global_select_limit: '1.0',
          project_id: 'hackanoodle',
          datasets: ['dataset1', 'additional_dataset'],
        },
      },
      {
        name: 'bq_test_2',
        kind: 'bigquery',
        tables: [],
        configuration: {
          service_account: {
            project_id: '***',
            client_email: '***@***.iam.gserviceaccount.com',
            private_key:
              '-----BEGIN PRIVATE KEY-----***-----END PRIVATE KEY-----\n',
          },
          global_select_limit: '1.0',
          project_id: 'hackanoodle',
          datasets: ['dataset1', 'additional_dataset'],
        },
      },
      {
        name: 'citus',
        kind: 'citus',
        tables: [
          {
            table: {
              schema: 'public',
              name: 'citus_test',
            },
            object_relationships: [
              {
                name: 'local_2',
                using: {
                  manual_configuration: {
                    remote_table: {
                      schema: 'public',
                      name: 'test_table',
                    },
                    insertion_order: null,
                    column_mapping: {
                      id: 'id',
                    },
                  },
                },
              },
            ],
            remote_relationships: [
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'citus2',
                    table: {
                      schema: 'public',
                      name: 'table2',
                    },
                    field_mapping: {
                      id: 'id',
                    },
                  },
                },
                name: 'citus2',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'default',
                    table: {
                      schema: 'public',
                      name: 'TTTT',
                    },
                    field_mapping: {
                      id: 'id',
                    },
                  },
                },
                name: 'local',
              },
              {
                definition: {
                  remote_field: {
                    countries: {
                      arguments: {},
                    },
                  },
                  hasura_fields: [],
                  remote_schema: 'countries',
                },
                name: 'rrr',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'citus2',
                    table: {
                      schema: 'public',
                      name: 'table2',
                    },
                    field_mapping: {
                      id: 'id',
                    },
                  },
                },
                name: 'sasdasfasfsdffff',
              },
              {
                definition: {
                  to_source: {
                    relationship_type: 'object',
                    source: 'bq_test',
                    table: {
                      dataset: 'dataset1',
                      name: 'bq_test_table',
                    },
                    field_mapping: {
                      id: 'PassengerId',
                    },
                  },
                },
                name: 'test_bq',
              },
            ],
          },
          {
            table: {
              schema: 'public',
              name: 'test_table',
            },
          },
        ],
        configuration: {
          connection_info: {
            use_prepared_statements: false,
            database_url:
              'postgres://postgres:postgrespassword@192.168.1.9:5434/postgres',
            isolation_level: 'read-committed',
          },
        },
      },
      {
        name: 'citus2',
        kind: 'citus',
        tables: [
          {
            table: {
              schema: 'public',
              name: 'table2',
            },
          },
        ],
        configuration: {
          connection_info: {
            use_prepared_statements: false,
            database_url:
              'postgres://postgres:postgrespassword@192.168.1.9:5434/postgres',
            isolation_level: 'read-committed',
          },
        },
      },
      {
        name: 'pg',
        kind: 'postgres',
        tables: [
          {
            table: {
              schema: 'public',
              name: 'test',
            },
          },
        ],
        configuration: {
          connection_info: {
            use_prepared_statements: false,
            database_url:
              'postgres://postgres:postgrespassword@192.168.1.9:5434/postgres',

            isolation_level: 'read-committed',
          },
        },
      },
    ],
    remote_schemas: [
      {
        name: 'countries',
        definition: {
          url: 'https://testremoteschema.com',
          timeout_seconds: 60,
        },
        comment: '',
      },
    ],
  },
};

// To test with any custom metadata
export const WithCustomMetadata = ({ metadata, ...args }: any) => {
  if (!metadata?.metadata?.sources) return 'Invalid Metadata';
  return (
    <>
      {metadata?.metadata?.sources.map((source: any) =>
        source?.tables?.map((table: TableEntry) => {
          if (
            table?.object_relationships ||
            table?.remote_relationships ||
            table?.array_relationships
          )
            return (
              <>
                <h2 className="text-xl p-2">{table?.table?.name}</h2>
                <RelationshipsTable tableDefinition={table} {...args} />
              </>
            );
          return null;
        })
      )}
    </>
  );
};

WithCustomMetadata.args = {
  metadata: customMetadata,
};
