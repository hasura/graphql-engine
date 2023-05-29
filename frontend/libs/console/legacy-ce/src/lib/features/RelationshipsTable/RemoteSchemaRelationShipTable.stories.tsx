import { StoryObj, Meta } from '@storybook/react';
import RemoteSchemaRelationshipTable from './RemoteSchemaRelationshipsTable';

const tableWithLegacyAndNewRemoteSchemaRelations: any = {
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
      table_name: 'tableName',
    },
    {
      definition: {
        to_remote_schema: {
          remote_field: { continents: { arguments: {} } },
        },
        remote_schema: 'countries',
      },
      name: 'continents',
      table_name: 'tableName',
    },
    {
      definition: {
        to_remote_schema: {
          remote_field: { countries: { arguments: {} } },
        },
        remote_schema: 'countries',
      },
      name: 'countries',
      table_name: 'tableName',
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
  title: 'Features/Relationships/RemoteSchemaRelationshipTable',
  component: RemoteSchemaRelationshipTable,
  argTypes: {
    onClick: { action: 'clicked' },
    onDelete: { action: 'clicked' },
    onEdit: { action: 'clicked' },
  },
} as Meta<typeof RemoteSchemaRelationshipTable>;

export const WithLgacyandNewRemoteRelationships: StoryObj<
  typeof RemoteSchemaRelationshipTable
> = {
  args: {
    remoteSchemaRels:
      tableWithLegacyAndNewRemoteSchemaRelations?.remote_relationships,
    remoteSchema: 'countries',
  },
};
