import { configSchema } from "./config"
import { CapabilitiesResponse } from "./types"

export const capabilitiesResponse: CapabilitiesResponse = {
  capabilities: { relationships: {} },
  configSchemas: configSchema
}
