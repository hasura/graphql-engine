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
    }
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
    }
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
    }
  },
  number: {
    comparison_operators: {
      _modulus_is_zero: 'number',
    },
    aggregate_functions: {
      max: 'number',
      min: 'number',
      sum: 'number'
    }
  },
  bool: {
    comparison_operators: {
      _and: 'bool',
      _or: 'bool',
      _nand: 'bool',
      _xor: 'bool',
    }
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
    scalar_types,
    queries: {},
    relationships: {},
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
