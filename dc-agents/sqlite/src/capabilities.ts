import { configSchema } from "./config"
import { CapabilitiesResponse } from "@hasura/dc-api-types"
import { envToBool } from "./util"

export const capabilitiesResponse: CapabilitiesResponse = {
  configSchemas: configSchema,
  capabilities: {
    queries: {
      supportsPrimaryKeys: true
    },
    relationships: {},
    explain: {},
    ... ( envToBool('METRICS') ?  { metrics: {} } : {} )
  },
}
