import { configSchema } from "./config"
import { CapabilitiesResponse } from "@hasura/dc-api-types"

export const capabilitiesResponse: CapabilitiesResponse = {
  capabilities: { relationships: {} },
  configSchemas: configSchema
}
