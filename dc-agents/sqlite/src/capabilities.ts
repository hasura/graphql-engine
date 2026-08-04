import { configSchema } from "./config"
import { DATASETS, METRICS, MUTATIONS } from "./environment"

import { CapabilitiesResponse, ScalarTypeCapabilities } from "@hasura/dc-api-types"

// NOTE: This should cover all possible schema types.
//       This type should be a subtype of ScalarType.
export type ScalarTypeKey
  = 'DateTime'
  | 'string'
  | 'number'
  | 'decimal'
  | 'bool'
  ;

// TODO: How can we ensure that we have covered all of the operator keys in the query module?
const scalar_types: Record<ScalarTypeKey, ScalarTypeCapabilities> = {
  DateTime: {
    comparison_operators: {
      _in_year: 'int'
    },
    graphql_type: "String",
  },
  string: {
    comparison_operators: {
      // See: https://www.sqlite.org/lang_expr.html #5
      _like: 'string',
      _glob: 'string',
      // _regexp: 'string', // TODO: Detect if REGEXP is supported
    },
    aggregate_functions: {
      max: 'string',
      min: 'string'
    },
    graphql_type: "String"
  },
  // TODO: Why do we need a seperate 'decimal' type?
  decimal: {
    comparison_operators: {
      _modulus_is_zero: 'decimal',
    },
    aggregate_functions: {
      max: 'decimal',
      min: 'decimal',
      sum: 'decimal'
    },
    update_column_operators: {
      inc: {
        argument_type: 'decimal'
      },
      dec: {
        argument_type: 'decimal'
      }
    },
    graphql_type: "Float"
  },
  number: {
    comparison_operators: {
      _modulus_is_zero: 'number',
    },
    aggregate_functions: {
      max: 'number',
      min: 'number',
      sum: 'number'
    },
    update_column_operators: {
      inc: {
        argument_type: 'number'
      },
      dec: {
        argument_type: 'number'
      }
    },
    graphql_type: "Float"
  },
  bool: {
    comparison_operators: {
      _and: 'bool',
      _or: 'bool',
      _nand: 'bool',
      _xor: 'bool',
    },
    graphql_type: "Boolean"
  }
};

export const capabilitiesResponse: CapabilitiesResponse = {
  display_name: 'Hasura SQLite',
  // release_name: 'Beta',
  config_schemas: configSchema,
  capabilities: {
    data_schema: {
      supports_primary_keys: true,
      supports_foreign_keys: true,
      column_nullability: "nullable_and_non_nullable",
    },
    post_schema: {},
    scalar_types,
    queries: {
      foreach: {}
    },
    relationships: {},
    interpolated_queries: {},
    comparisons: {
      subquery: {
        supports_relations: true
      }
    },
    ... (
      MUTATIONS
        ? {
          mutations: {
            atomicity_support_level: "heterogeneous_operations",
            insert: { supports_nested_inserts: true },
            update: {},
            delete: {},
            returning: {},
          }
        }
        : {}
    ),
    explain: {},
    raw: {},
    ... (DATASETS ? { datasets: {} } : {}),
    ... (METRICS ? { metrics: {} } : {})
  },
}
