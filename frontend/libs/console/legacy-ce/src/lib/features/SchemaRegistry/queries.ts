import { parse as gql } from 'graphql';

export const FETCH_REGSITRY_SCHEMAS_QUERY = gql(`
query fetchRegistrySchemas($projectId: uuid!) {
  schema_registry_dumps(
    where: {_and: [{project_id: {_eq: $projectId}, hasura_schema_role: {_eq: "admin"}}]},
    order_by: {change_recorded_at: desc}
  ) {
    change_recorded_at
    schema_hash
    entry_hash
    id
    sibling_schemas {
      id
      entry_hash
      change_recorded_at
      created_at
      hasura_schema_role
      schema_sdl
      diff_with_previous_schema {
        current_schema_hash
        former_schema_hash
        former_schema_id
        current_schema_id
        schema_diff_data
      }
    }
  }
}

`);

export const FETCH_REGSITRY_SCHEMA_QUERY = gql(`
query fetchRegistrySchema ($schemaId: uuid!) {
  schema_registry_dumps (
    where: {
      id: {
        _eq: $schemaId
      }
    }
  ) {
    id
    entry_hash
    schema_hash
    change_recorded_at
    created_at
    hasura_schema_role
    schema_sdl
    diff_with_previous_schema {
      current_schema_hash
      former_schema_hash
      former_schema_id
      current_schema_id
      schema_diff_data
    }
  }
}
`);
