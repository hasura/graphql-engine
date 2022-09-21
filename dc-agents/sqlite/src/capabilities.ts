import { configSchema } from "./config"
import { CapabilitiesResponse } from "@hasura/dc-api-types"
import { envToBool } from "./util"

export const capabilitiesResponse: CapabilitiesResponse = {
  config_schemas: configSchema,
  capabilities: {
    queries: {
      supports_primary_keys: true
    },
    relationships: {},
    comparisons: {
      cross_table: {
        supports_relations: true
      }
    },
    explain: {},
    ... ( envToBool('METRICS') ?  { metrics: {} } : {} )
  },
}
