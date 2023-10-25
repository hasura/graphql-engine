export default {
  capabilities: {
    comparisons: {
      subquery: {
        supports_relations: true,
      },
    },
    data_schema: {
      supports_primary_keys: true,
      supports_schemaless_tables: true,
    },
    explain: {},
    queries: {
      foreach: {},
    },
    relationships: {},
    scalar_types: {
      binData: {},
      bool: {
        graphql_type: 'Boolean',
      },
      date: {},
      dbPointer: {},
      decimal: {
        graphql_type: 'String',
      },
      double: {
        aggregate_functions: {
          avg: 'Float',
          count: 'Int',
          max: 'Float',
          min: 'Float',
          sum: 'Float',
        },
        graphql_type: 'Float',
      },
      function: {},
      int: {
        graphql_type: 'Int',
      },
      javascript: {},
      javascriptWithScope: {},
      long: {
        graphql_type: 'String',
      },
      maxKey: {},
      md5: {},
      minKey: {},
      null: {},
      objectId: {},
      regex: {},
      string: {
        graphql_type: 'String',
      },
      symbol: {},
      timestamp: {},
      undefined: {},
      uuid: {},
    },
  },
  config_schema_response: {
    config_schema: {
      properties: {
        connection: {
          description: 'connection URI of a MongoDB database',
          type: 'string',
        },
        db: {
          description: 'The mongo database name',
          type: 'string',
        },
      },
      required: ['connection', 'db'],
      type: 'object',
    },
    other_schemas: {},
  },
  display_name: 'MongoDB',
  options: {
    uri: 'http://host.docker.internal:3000',
  },
  release_name: 'experimental',
};
