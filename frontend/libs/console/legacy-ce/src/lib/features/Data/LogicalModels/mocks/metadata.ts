import { LogicalModel, Metadata } from '../../../hasura-metadata-types';

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
    {
      arguments: {},
      code: 'select "CustomerId", "BillingCity" as "City", "BillingCountry" as "Country" from "public"."Invoice"',
      returns: 'customer_location_model',
      root_field_name: 'customer_location',
    },
    {
      arguments: {},
      code: 'select "CustomerId" as "Id", "FirstName" as "Name" from "public"."Customer"',
      object_relationships: [
        {
          name: 'location',
          using: {
            column_mapping: {
              Id: 'CustomerId',
            },
            insertion_order: null,
            remote_native_query: 'customer_location',
          },
        },
      ],
      returns: 'customer_model',
      root_field_name: 'customer_native_query',
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

const testModels: Record<string, LogicalModel[]> = {
  postgres: [
    {
      fields: [
        {
          name: 'one',
          type: { scalar: 'string', nullable: true },
        },
        {
          name: 'two',
          type: { scalar: 'string', nullable: true },
        },
      ],
      name: 'hello_world',
    },
    {
      fields: [
        {
          name: 'one',
          type: { scalar: 'string', nullable: true },
        },
        {
          name: 'two',
          type: { scalar: 'string', nullable: true },
        },
      ],
      name: 'hello_world2',
    },
    // for testing relationships:
    {
      fields: [
        {
          name: 'CustomerId',
          type: {
            nullable: false,
            scalar: 'integer',
          },
        },
        {
          name: 'City',
          type: {
            nullable: true,
            scalar: 'varchar',
          },
        },
        {
          name: 'Country',
          type: {
            nullable: true,
            scalar: 'varchar',
          },
        },
      ],
      name: 'customer_location_model',
    },
    {
      fields: [
        {
          name: 'Id',
          type: {
            nullable: false,
            scalar: 'integer',
          },
        },
        {
          name: 'Name',
          type: {
            nullable: true,
            scalar: 'varchar',
          },
        },
      ],
      name: 'customer_model',
    },
  ],
  mssql: [
    {
      fields: [
        {
          name: 'one',
          type: { scalar: 'string', nullable: true },
        },
        {
          name: 'two',
          type: { scalar: 'string', nullable: true },
        },
      ],
      name: 'hello_mssql_unused',
    },
    {
      fields: [
        {
          name: 'one',
          type: { scalar: 'string', nullable: true },
        },
        {
          name: 'two',
          type: { scalar: 'string', nullable: true },
        },
      ],
      name: 'hello_mssql',
    },
    {
      fields: [
        {
          name: 'one',
          type: { scalar: 'string', nullable: true },
        },
        {
          name: 'two',
          type: { scalar: 'string', nullable: true },
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

export const buildMetadata = ({
  postgres,
  mssql,
}: MockMetadataOptions): Metadata => {
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
          logical_models: mssql?.models ? (testModels.mssql as any) : [],
          configuration: {
            connection_info: {
              connection_string: '',
            },
          },
        },
        {
          name: 'postgres',
          kind: 'postgres',
          tables: [],
          native_queries: postgres?.queries ? testQueries.postgres : [],
          logical_models: postgres?.models ? (testModels.postgres as any) : [],
          configuration: {
            connection_info: {
              database_url: '',
            },
          },
        },
      ],
    },
  };
};
