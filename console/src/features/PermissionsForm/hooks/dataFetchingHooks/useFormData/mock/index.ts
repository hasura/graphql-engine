import { TableColumn } from '@/features/DataSource';
import { Metadata } from '@/features/MetadataAPI';

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
                  columns: ['ArtistId', 'Name'],
                  filter: {},
                  allow_aggregations: true,
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

export const input: Input = {
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
};
