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
  },
  graphql_type: 'String'
}

const stringCapabilities: ScalarTypeCapabilities = {
  aggregate_functions: {
    longest: 'string',
    shortest: 'string',
    min: 'string',
    max: 'string'
  },
  graphql_type: 'String'
}

const numberCapabilities: ScalarTypeCapabilities = {
  aggregate_functions: {
    max: 'number',
    min: 'number',
    stddev: 'number',
    stddev_pop: 'number',
    stddev_samp: 'number',
    sum: 'number',
    var_pop: 'number',
    var_samp: 'number',
    variance: 'number'
  },
  update_column_operators: {
    inc: {
      argument_type: 'number'
    }
  },
  graphql_type: 'Float'
}

const scalarTypes: ScalarTypesCapabilities = {
  DateTime: dateTimeCapabilities,
  string: stringCapabilities,
  number: numberCapabilities
}

const capabilities: Capabilities = {
  data_schema: {
    supports_primary_keys: true,
    supports_foreign_keys: true,
    column_nullability: "nullable_and_non_nullable",
  },
  post_schema: {},
  queries: {
    foreach: {},
    redaction: {},
  },
  relationships: {},
  comparisons: {
    subquery: {
      supports_relations: true
    }
  },
  scalar_types: scalarTypes,
  datasets: {},
  user_defined_functions: {}
}

export const capabilitiesResponse: CapabilitiesResponse = {
  capabilities: capabilities,
  config_schemas: configSchema
}
