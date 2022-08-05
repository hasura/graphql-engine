import { ConfigSchemaResponse, configSchema } from "./config"

export type Relationships = {}

export type Capabilities = {
  relationships: Relationships
}

export type CapabilitiesResponse = {
  capabilities: Capabilities,
  configSchemas: ConfigSchemaResponse,
}

export const capabilitiesResponse: CapabilitiesResponse = {
  capabilities: { relationships: {}},
  configSchemas: configSchema
}
