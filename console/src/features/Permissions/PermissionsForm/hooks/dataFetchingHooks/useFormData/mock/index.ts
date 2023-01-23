import { TableColumn } from '@/features/DataSource';
import { Metadata } from '@/features/hasura-metadata-types';
import { createDefaultValues } from '../createDefaultValues';
import { schema } from '../../../../components/RowPermissionsBuilder/mocks';

interface Input {
  dataSourceName: string;
  table: unknown;
  metadata: Metadata;
  tableColumns: TableColumn[];
}

const metadata: Metadata = {
  resource_version: 1,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'sqlite',
        kind: 'sqlite',
        tables: [
          {
            table: ['Album'],
          },
          {
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
                  allow_aggregations: true,
                  limit: 3,
                },
              },
            ],
          },
        ],
        configuration: {
          template: null,
          timeout: null,
          value: {
            db: '/chinook.db',
            include_sqlite_meta_tables: false,
          },
        },
      },
    ],
    backend_configs: {
      dataconnector: {
        sqlite: {
          uri: 'http://host.docker.internal:8100',
        },
      },
    },
  },
};

export const formDataInput: Input = {
  dataSourceName: 'sqlite',
  table: ['Artist'],
  metadata,
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
      consoleDataType: 'number',
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
      consoleDataType: 'string',
    },
  ],
};

export const defaultValuesInput: Parameters<typeof createDefaultValues>[0] = {
  dataSourceName: 'sqlite',
  table: ['Artist'],
  metadata,
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
      consoleDataType: 'number',
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
      consoleDataType: 'string',
    },
  ],
  schema,
  defaultQueryRoot: 'Artist',
};
