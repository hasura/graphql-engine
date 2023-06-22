import { parse as gql } from 'graphql';

export const FETCH_REGISTRY_SCHEMAS_QUERY = gql(`
query fetchRegistrySchemas($projectId: uuid!, $cursor: timestamptz!, $limit: Int!) {
  schema_registry_dumps(
    where: {_and: [{project_id: {_eq: $projectId}, hasura_schema_role: {_eq: "admin"}}, {change_recorded_at: {_lt: $cursor}}]},
    order_by: {change_recorded_at: desc}
    limit: $limit
  ) {
    change_recorded_at
    schema_hash
    entry_hash
    id
    metadata_resource_version
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

export const FETCH_REGISTRY_SCHEMA_QUERY = gql(`
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

export const FETCH_ALERT_CONFIG = gql(`
query QueryAlerts($projectId: uuid!) {
  schema_registry_alerts(where: {project_id: {_eq: $projectId}}) {
    config
    alert_type
    id
    meta
    project_id
    slack_webhook
  }
}
`);

export const SET_ALERT_CONFIG = gql(`
mutation UpsertEmailAlertConfig($projectId: uuid, $config: jsonb) {
  insert_schema_registry_alerts_one(object: {alert_type: email, config: $config, project_id: $projectId}, on_conflict: {constraint: schema_registry_alerts_project_id_alert_type_key, update_columns: config}) {
    id
    project_id
    alert_type
    config
  }
}
`);
