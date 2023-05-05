const testQueries = {
  postgres: [
    {
      arguments: {},
      code: "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\")\n",
      returns: 'hello_world',
      root_field_name: 'hello_world_function',
    },
    {
      arguments: {},
      code: "SELECT * FROM (VALUES ('hello', 'world2'), ('welcome', 'friend')) as t(\"one\", \"two\")\n",
      returns: 'hello_world2',
      root_field_name: 'hello_world_function2',
    },
  ],
  mssql: [
    {
      arguments: {},
      code: "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\")\n",
      returns: 'hello_mssql',
      root_field_name: 'hello_mssql_function',
    },
    {
      arguments: {},
      code: "SELECT * FROM (VALUES ('hello', 'world2'), ('welcome', 'friend')) as t(\"one\", \"two\")\n",
      returns: 'hello_mssql2',
      root_field_name: 'hello_mssql_function2',
    },
  ],
};

const testModels = {
  postgres: [
    {
      fields: [
        {
          name: 'one',
          nullable: false,
          type: 'text',
        },
        {
          name: 'two',
          nullable: false,
          type: 'text',
        },
      ],
      name: 'hello_world',
    },
    {
      fields: [
        {
          name: 'one',
          nullable: false,
          type: 'text',
        },
        {
          name: 'two',
          nullable: false,
          type: 'text',
        },
      ],
      name: 'hello_world2',
    },
  ],
  mssql: [
    {
      fields: [
        {
          name: 'one',
          nullable: false,
          type: 'text',
        },
        {
          name: 'two',
          nullable: false,
          type: 'text',
        },
      ],
      name: 'hello_mssql',
    },
    {
      fields: [
        {
          name: 'one',
          nullable: false,
          type: 'text',
        },
        {
          name: 'two',
          nullable: false,
          type: 'text',
        },
      ],
      name: 'hello_mssql2',
    },
  ],
};

export type MockMetadataOptions = {
  postgres?: { models?: boolean; queries?: boolean };
  mssql?: { models?: boolean; queries?: boolean };
};

export const metadata = ({ postgres, mssql }: MockMetadataOptions) => {
  return {
    resource_version: 528,
    metadata: {
      version: 3,
      sources: [
        {
          name: 'mssql',
          kind: 'mssql',
          tables: [],
          native_queries: mssql?.queries ? testQueries.mssql : [],
          logical_models: mssql?.models ? testModels.mssql : [],
        },
        {
          name: 'postgres',
          kind: 'postgres',
          tables: [],
          native_queries: postgres?.queries ? testQueries.postgres : [],
          logical_models: postgres?.models ? testModels.postgres : [],
        },
      ],
    },
  };
};
