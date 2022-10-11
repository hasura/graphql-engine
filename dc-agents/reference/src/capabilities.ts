import { configSchema } from "./config"
import { Capabilities, CapabilitiesResponse, ScalarTypeCapabilities, ScalarTypesCapabilities } from "@hasura/dc-api-types"

const schemaDoc: string =
  `scalar DateTime

input DateTimeComparisons {
  same_day_as: DateTime
  in_year: Int
}`

const dateTimeCapabilities: ScalarTypeCapabilities = {
  comparison_type: 'DateTimeComparisons'
}

const scalarTypes: ScalarTypesCapabilities = {
  DateTime: dateTimeCapabilities
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
  graphql_schema: schemaDoc,
  scalar_types: scalarTypes
}

export const capabilitiesResponse: CapabilitiesResponse = {
  capabilities: capabilities,
  config_schemas: configSchema
}
