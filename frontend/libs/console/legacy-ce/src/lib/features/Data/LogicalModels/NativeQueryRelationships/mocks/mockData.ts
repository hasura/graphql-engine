import { Metadata } from '../../../../hasura-metadata-types';

export const mockMetadata: Metadata = {
  resource_version: 24,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'chinook',
        kind: 'postgres',
        tables: [],
        native_queries: [
          {
            arguments: {
              shouting_title: {
                nullable: false,
                type: 'boolean',
              },
            },
            code: 'SELECT id, author_id, (CASE WHEN {{shouting_title}}=true THEN UPPER(title) ELSE title END) as title, content FROM article',
            returns: 'article_model',
            root_field_name: 'get_article',
          },
          {
            arguments: {
              shouting_name: {
                nullable: false,
                type: 'boolean',
              },
            },
            object_relationships: [
              {
                name: 'author_details',
                using: {
                  column_mapping: {
                    id: 'author_id',
                  },
                  insertion_order: null,
                  remote_native_query: 'get_author_details',
                },
              },
            ],
            array_relationships: [
              {
                name: 'articles',
                using: {
                  column_mapping: {
                    id: 'author_id',
                  },
                  insertion_order: null,
                  remote_native_query: 'get_article',
                },
              },
            ],
            code: 'SELECT id, (CASE WHEN {{shouting_name}}=true THEN UPPER(name) ELSE name END) as name FROM author',
            returns: 'author_model',
            root_field_name: 'get_authors',
          },
        ],
        logical_models: [
          {
            fields: [
              {
                name: 'id',
                type: {
                  nullable: false,
                  scalar: 'integer',
                },
              },
              {
                name: 'author_id',
                type: {
                  nullable: false,
                  scalar: 'integer',
                },
              },
              {
                name: 'title',
                type: {
                  nullable: false,
                  scalar: 'text',
                },
              },
              {
                name: 'content',
                type: {
                  nullable: false,
                  scalar: 'text',
                },
              },
            ],
            name: 'article_model',
          },
          {
            fields: [
              {
                name: 'id',
                type: {
                  nullable: false,
                  scalar: 'integer',
                },
              },
              {
                name: 'name',
                type: {
                  nullable: false,
                  scalar: 'text',
                },
              },
              {
                name: 'articles',
                type: {
                  array: {
                    logical_model: 'article_model',
                    nullable: false,
                  },
                  nullable: false,
                },
              },
            ],
            name: 'author_model',
          },
        ],
        configuration: {
          connection_info: {
            database_url:
              'postgres://postgres:test@host.docker.internal:6001/chinook',
            isolation_level: 'read-committed',
            use_prepared_statements: false,
          },
        },
      },
    ],
  },
};
