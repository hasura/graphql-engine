import { configSchema } from "./config"
import { Capabilities, CapabilitiesResponse, ScalarTypeCapabilities, ScalarTypesCapabilities } from "@hasura/dc-api-types"

const dateTimeCapabilities: ScalarTypeCapabilities = {
  comparison_operators: {
    same_day_as: 'DateTime',
    in_year: 'Int'
  },
  aggregate_functions: {
    max: 'DateTime',
    min: 'DateTime'
  }
}

const stringCapabilities: ScalarTypeCapabilities = {
  aggregate_functions: {
    longest: 'string',
    shortest: 'string'
  }
}

const scalarTypes: ScalarTypesCapabilities = {
  DateTime: dateTimeCapabilities,
  string: stringCapabilities
}

const capabilities: Capabilities = {
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
  scalar_types: scalarTypes
}

export const capabilitiesResponse: CapabilitiesResponse = {
  capabilities: capabilities,
  config_schemas: configSchema
}
