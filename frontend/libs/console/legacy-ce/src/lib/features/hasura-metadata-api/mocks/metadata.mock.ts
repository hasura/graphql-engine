import { Metadata, MetadataTable, Source } from '../../hasura-metadata-types';

export const metadataTables: MetadataTable[] = [
  {
    table: ['Album'],
  },
  {
    table: ['Artist'],
  },
  {
    table: ['Genre'],
  },
  {
    table: ['Track'],
  },
];

export const metadataSource: Source = {
  name: 'sqlite_test',
  kind: 'sqlite',
  tables: metadataTables,
  configuration: {
    template: null,
    timeout: null,
    value: {
      db: './chinook.db',
      explicit_main_schema: false,
      include_sqlite_meta_tables: false,
      tables: ['Album', 'Artist', 'Genre', 'Track'],
    },
  },
};

export const metadataWithoutSources: Metadata = {
  resource_version: 1,
  metadata: { version: 3, sources: [] },
};

// this isn't technically possible as long as server types are consistent
export const metadataWithResourceVersion = {
  metadata: { version: 3, sources: [] },
};

export const metadataWithSourcesAndTables: Metadata = {
  resource_version: 73,
  metadata: {
    version: 3,
    sources: [metadataSource],
    backend_configs: {
      dataconnector: {
        sqlite: {
          uri: 'http://host.docker.internal:8100',
        },
      },
    },
  },
};
