import { FastifyRequest } from "fastify"
import { ConfigSchemaResponse } from "@hasura/dc-api-types"

export type Casing = "pascal_case" | "lowercase";

export type Config = {
  tables: string[] | null
  schema: string | null
  db: string | null
  table_name_casing: Casing
  column_name_casing: Casing
}

export const getConfig = (request: FastifyRequest): Config => {
  const configHeader = request.headers["x-hasura-dataconnector-config"];
  const rawConfigJson = Array.isArray(configHeader) ? configHeader[0] : configHeader ?? "{}";
  const config = JSON.parse(rawConfigJson);
  return {
    tables: config.tables ?? null,
    schema: config.schema ?? null,
    db: config.db ?? null,
    table_name_casing: config.table_name_casing ?? "pascal_case",
    column_name_casing: config.column_name_casing ?? "pascal_case",
  }
}

export const configSchema: ConfigSchemaResponse = {
  config_schema: {
    type: "object",
    nullable: false,
    properties: {
      tables: {
        description: "List of tables to make available in the schema and for querying",
        type: "array",
        items: { $ref: "#/other_schemas/TableName" },
        nullable: true
      },
      schema: {
        description: "Name of the schema to place the tables in. Omit to have no schema for the tables",
        type: "string",
        nullable: true
      },
      db: {
        description: "Name of the db. Omit to use the default db.",
        type: "string",
        nullable: true
      },
      table_name_casing: {
        $ref: "#/other_schemas/Casing"
      },
      column_name_casing: {
        $ref: "#/other_schemas/Casing"
      },
    }
  },
  other_schemas: {
    TableName: {
      nullable: false,
      type: "string"
    },
    Casing: {
      enum: [
        "pascal_case",
        "lowercase"
      ],
      type: "string",
      nullable: true
    }
  }
}
