import { configSchema } from "./config"
import { CapabilitiesResponse } from "./types"
import { envToBool } from "./util"

export const capabilitiesResponse: CapabilitiesResponse = {
  configSchemas: configSchema,
  capabilities: {
    filtering: {
      booleanOperators: {},
      comparisonOperators: {}
    },
    queries: {
      supportsPrimaryKeys: true
    },
    relationships: {},
    ... ( envToBool('METRICS') ?  { metrics: {} } : {} )
  },
}

