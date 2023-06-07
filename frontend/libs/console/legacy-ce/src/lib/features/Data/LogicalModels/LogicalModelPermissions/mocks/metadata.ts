export default {
  version: 3,
  sources: [
    {
      name: 'MS',
      kind: 'mssql',
      tables: [],
      logical_models: [
        {
          fields: [],
          name: 'Model',
        },
      ],
      configuration: {
        connection_info: {
          connection_string:
            'Driver={ODBC Driver 18 for SQL Server};Server=tcp:host.docker.internal,1433;Database=tempdb;Uid=sa;Pwd=Password!;Encrypt=optional',
          pool_settings: {
            idle_timeout: 5,
            max_connections: null,
            total_max_connections: null,
          },
        },
      },
    },
    {
      name: 'Postgres',
      kind: 'postgres',
      tables: [
        {
          table: {
            name: 'Album',
            schema: 'public',
          },
          object_relationships: [
            {
              name: 'Album_Artist',
              using: {
                foreign_key_constraint_on: 'ArtistId',
              },
            },
            {
              name: 'D',
              using: {
                manual_configuration: {
                  column_mapping: {
                    ArtistId: 'ArtistId',
                  },
                  insertion_order: null,
                  remote_table: {
                    name: 'Artist',
                    schema: 'public',
                  },
                },
              },
            },
          ],
          remote_relationships: [
            {
              definition: {
                to_source: {
                  field_mapping: {
                    ArtistId: 'ArtistId',
                  },
                  relationship_type: 'object',
                  source: 'Postgres',
                  table: {
                    name: 'Artist',
                    schema: 'public',
                  },
                },
              },
              name: 'A',
            },
            {
              definition: {
                to_source: {
                  field_mapping: {
                    ArtistId: 'ArtistId',
                  },
                  relationship_type: 'object',
                  source: 'Postgres',
                  table: {
                    name: 'Artist',
                    schema: 'public',
                  },
                },
              },
              name: 'B',
            },
            {
              definition: {
                to_source: {
                  field_mapping: {
                    ArtistId: 'ArtistId',
                  },
                  relationship_type: 'object',
                  source: 'Postgres',
                  table: {
                    name: 'Artist',
                    schema: 'public',
                  },
                },
              },
              name: 'C',
            },
          ],
        },
        {
          table: {
            name: 'Artist',
            schema: 'public',
          },
        },
      ],
      logical_models: [
        {
          fields: [
            {
              name: 'id',
              nullable: true,
              type: 'integer',
            },
            {
              name: 'name',
              nullable: true,
              type: 'text',
            },
          ],
          name: 'LogicalModel',
          select_permissions: [
            {
              permission: {
                columns: ['id'],
                filter: {},
              },
              role: 'editor',
            },
          ],
        },
        {
          fields: [],
          name: 'M',
          select_permissions: [
            {
              permission: {
                columns: [],
                filter: {
                  _or: [{}],
                },
              },
              role: 'M',
            },
            {
              permission: {
                columns: [],
                filter: {
                  _and: [{}],
                },
              },
              role: 'reader',
            },
          ],
        },
      ],
      configuration: {
        connection_info: {
          database_url: 'postgres://postgres:pass@postgres:5432/chinook',
          isolation_level: 'read-committed',
          use_prepared_statements: false,
        },
      },
    },
  ],
};
