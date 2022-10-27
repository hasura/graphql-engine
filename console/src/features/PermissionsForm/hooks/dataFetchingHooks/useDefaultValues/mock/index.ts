import { createDefaultValues } from '../..';
import { schema } from '../../../../components/RowPermissionsBuilder/mocks';

export const input: Parameters<typeof createDefaultValues>[0] = {
  queryType: 'select' as const,
  roleName: 'user',
  tableColumns: [
    {
      name: 'ArtistId',
      dataType: 'number',
      nullable: false,
      isPrimaryKey: true,
      graphQLProperties: {
        name: 'ArtistId',
        scalarType: 'decimal',
      },
    },
    {
      name: 'Name',
      dataType: 'string',
      nullable: true,
      isPrimaryKey: false,
      graphQLProperties: {
        name: 'Name',
        scalarType: 'String',
      },
    },
  ],
  selectedTable: {
    table: ['Artist'],
    select_permissions: [
      {
        role: 'user',
        permission: {
          columns: ['Name'],
          filter: {
            ArtistId: {
              _gt: 5,
            },
          },
          limit: 3,
          allow_aggregations: true,
        },
      },
    ],
  },
  schema,
};
