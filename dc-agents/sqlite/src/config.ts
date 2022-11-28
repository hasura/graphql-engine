import { FastifyRequest } from "fastify"
import { ConfigSchemaResponse } from "@hasura/dc-api-types"

export type Config = {
  db: string,
  explicit_main_schema: Boolean,
  tables: String[] | null,
  meta: Boolean
}

export const getConfig = (request: FastifyRequest): Config => {
  const config = tryGetConfig(request);
  if (config === null) {
    throw new Error("X-Hasura-DataConnector-Config header must specify db");
  }
  return config;
}

export const tryGetConfig = (request: FastifyRequest): Config | null => {
  const configHeader = request.headers["x-hasura-dataconnector-config"];
  const rawConfigJson = Array.isArray(configHeader) ? configHeader[0] : configHeader ?? "{}";
  const config = JSON.parse(rawConfigJson);

  if(config.db == null) {
    return null;
  }

  return {
    db: config.db,
    explicit_main_schema: config.explicit_main_schema ?? false,
    tables: config.tables ?? null,
    meta: config.include_sqlite_meta_tables ?? false
  }
}

export const configSchema: ConfigSchemaResponse = {
  config_schema: {
    type: "object",
    nullable: false,
    required: ["db"],
    properties: {
      db: {
        description: "The SQLite database file to use.",
        type: "string"
      },
      explicit_main_schema: {
        description: "Prefix all tables with the 'main' schema",
        type: "boolean",
        nullable: true,
        default: false
      },
      tables: {
        description: "List of tables to make available in the schema and for querying",
        type: "array",
        items: { $ref: "#/other_schemas/TableName" },
        nullable: true
      },
      include_sqlite_meta_tables: {
        description: "By default index tables, etc are not included, set this to true to include them.",
        type: "boolean",
        nullable: true
      },
      DEBUG: {
        description: "For debugging.",
        type: "object",
        additionalProperties: true,
        nullable: true
      }
    }
  },
  other_schemas: {
    TableName: {
      nullable: false,
      type: "string"
    }
  }
}
