import { RemoteRelationship } from '@/metadata/types';

export const dbToRemoteSchemaRelationships = {
  table: {
    schema: 'public',
    name: 'user',
  },
  remote_relationships: ([
    {
      definition: {
        to_remote_schema: {
          remote_field: {
            test: {
              arguments: {
                where: {
                  id: {
                    _eq: '$id',
                  },
                },
              },
            },
          },
          remote_schema: 'remoteSchema3',
          lhs_fields: ['id'],
        },
      },
      name: 'new_payload',
    },
    {
      definition: {
        remote_field: {
          test: {
            arguments: {
              where: {
                id: {
                  _eq: '$id',
                },
              },
            },
          },
        },
        hasura_fields: ['id'],
        remote_schema: 'remoteSchema3',
      },
      name: 't',
    },
  ] as unknown) as RemoteRelationship[],
};
