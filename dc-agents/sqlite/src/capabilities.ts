import { configSchema } from "./config"
import { CapabilitiesResponse } from "@hasura/dc-api-types"
import { envToBool } from "./util"

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
    scalar_types: {
      DateTime: {}
    },
    queries: {},
    relationships: {},
    comparisons: {
      subquery: {
        supports_relations: true
      }
    },
    ... (
      envToBool('MUTATIONS')
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
    ... ( envToBool('METRICS') ?  { metrics: {} } : {} )
  },
}
