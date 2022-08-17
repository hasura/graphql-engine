import { FastifyRequest } from "fastify"
import { ConfigSchemaResponse } from "./types"

export type Config = {
  db: string,
  tables: String[] | null,
  meta: Boolean
}

export const getConfig = (request: FastifyRequest): Config => {
  const configHeader = request.headers["x-hasura-dataconnector-config"];
  const rawConfigJson = Array.isArray(configHeader) ? configHeader[0] : configHeader ?? "{}";
  const config = JSON.parse(rawConfigJson);
  return {
    db: config.db,
    tables: config.tables ?? null,
    meta: config.include_sqlite_meta_tables ?? false
  }
}

export const configSchema: ConfigSchemaResponse = {
  configSchema: {
    type: "object",
    nullable: false,
    properties: {
      db: {
        description: "The SQLite database file to use.",
        type: "string"
      },
      tables: {
        description: "List of tables to make available in the schema and for querying",
        type: "array",
        items: { $ref: "#/otherSchemas/TableName" },
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
  otherSchemas: {
    TableName: {
      nullable: false,
      type: "string"
    }
  }
}
