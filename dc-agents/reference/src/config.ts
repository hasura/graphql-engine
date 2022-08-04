import { FastifyRequest } from "fastify"
import { ConfigSchemaResponse } from "./types"

export type Config = {
  tables: string[] | null
  schema: string | null
}

export const getConfig = (request: FastifyRequest): Config => {
  const configHeader = request.headers["x-hasura-dataconnector-config"];
  const rawConfigJson = Array.isArray(configHeader) ? configHeader[0] : configHeader ?? "{}";
  const config = JSON.parse(rawConfigJson);
  return {
    tables: config.tables ?? null,
    schema: config.schema ?? null
  }
}

export const configSchema: ConfigSchemaResponse = {
  configSchema: {
    type: "object",
    nullable: false,
    properties: {
      tables: {
        description: "List of tables to make available in the schema and for querying",
        type: "array",
        items: { $ref: "#/otherSchemas/TableName" },
        nullable: true
      },
      schema: {
        description: "Name of the schema to place the tables in. Omit to have no schema for the tables",
        type: "string",
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
