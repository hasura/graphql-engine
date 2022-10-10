import { configSchema } from "./config"
import { CapabilitiesResponse } from "@hasura/dc-api-types"
import { envToBool } from "./util"

export const capabilitiesResponse: CapabilitiesResponse = {
  config_schemas: configSchema,
  capabilities: {
    data_schema: {
      supports_primary_keys: true,
      supports_foreign_keys: true,
      column_nullability: "nullable_and_non_nullable",
    },
    queries: {},
    relationships: {},
    comparisons: {
      subquery: {
        supports_relations: true
      }
    },
    explain: {},
    raw: {},
    ... ( envToBool('METRICS') ?  { metrics: {} } : {} )
  },
}
