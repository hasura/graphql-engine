import { configSchema } from "./config"
import { METRICS, MUTATIONS } from "./environment"

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

// TODO: Should we prefix operators with _ like _eq, or just for symbolic operators?
// TODO: How to model ISNULL, NOTNULL, IN
// TODO: How to apply functions to column arguments? E.g. a.bits = b.bits & b.mask
// TODO: How to reuse operators such as = across different types without enumerating them all?
// TODO: Other operators. See: https://www.tutorialspoint.com/sqlite/sqlite_operators.htm
//       Should we explicitly include the default included operators?
// NOTE: See `function bop_op` for most query processing of these operators
// 
function standardOperators(t: string): Record<string, string> {
  return {
    _eq: t,  // ==	Checks if the values of two operands are equal or not, if yes then the condition becomes true.	(a == b) is not true.
    // =	Checks if the values of two operands are equal or not, if yes then the condition becomes true.	(a = b) is not true.
    _gt: t,  // >	Checks if the values of the left operand is greater than the value of the right operand, if yes then the condition becomes true.	(a > b) is not true.
    _gte: t, // >=	Checks if the value of the left operand is greater than or equal to the value of the right operand, if yes then the condition becomes true.	(a >= b) is not true.
    // TODO: _in
    // TODO: _is_null
    _lt: t,  // <	Checks if the values of the left operand is less than the value of the right operand, if yes then the condition becomes true.	(a < b) is true.
    _lte: t, // <=	Checks if the value of the left operand is less than or equal to the value of the right operand, if yes then the condition becomes true.	(a <= b) is true.
    _neq: t, // !=	Checks if the values of two operands are equal or not, if the values are not equal, then the condition becomes true.	(a != b) is true.
    // <>	Checks if the values of two operands are equal or not, if the values are not equal, then the condition becomes true.	(a <> b) is true.

    // TODO: The following operators are listed in the documentation but throw errors when used...
    // _nlt: t, // !<	Checks if the value of the left operand is not less than the value of the right operand, if yes then the condition becomes true.	(a !< b) is false.
    // _ngt: t  // !>	Checks if the value of the left operand is not greater than the value of the right operand, if yes then the condition becomes true.
  }
}

// TODO: How can we ensure that we have covered all of the operator keys in the query module?
const scalar_types: Record<ScalarTypeKey, ScalarTypeCapabilities> = {
  DateTime: {
    comparison_operators: {
      _in_year: 'int',
      ...standardOperators('DateTime')
    }
  },
  string: {
    comparison_operators: {
      // See: https://www.sqlite.org/lang_expr.html #5
      _like: 'string',
      _glob: 'string',
      // _regexp: 'string', // TODO: Detect if REGEXP is supported
      ...standardOperators('string')
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
      ...standardOperators('decimal')
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
      ...standardOperators('number')
    },
    aggregate_functions: {
      max: 'number',
      min: 'number',
      sum: 'number'
    }
  },
  bool: {
    comparison_operators: {
      // TODO: Should we include the standard boolean operators for column comparisons?
      _and: 'bool',
      _or: 'bool',
      _nand: 'bool',
      _xor: 'bool',
      ...standardOperators('bool')
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
    ... (METRICS ? { metrics: {} } : {})
  },
}
