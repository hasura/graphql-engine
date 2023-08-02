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
    schema_tags {
      id
      name
      entry_hash
      color
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
query fetchAlertConfig($projectId: uuid!, $type: alert_service_type_enum!) {
  alert_config_service(where: {project_id: {_eq: $projectId}, type: {_eq: $type}}) {
    project_id
    type
    metadata
    rules
  }
}
`);

export const SET_ALERT_CONFIG = gql(`
mutation UpsertAlertConfig($projectId: uuid, $rules: jsonb) {
  insert_alert_config(objects: {alert_types: {data: {type: "SchemaRegistryUpdates"}, on_conflict: {constraint: alert_config_alert_type_pkey, update_columns: type}}, project_id: $projectId, enabled: true, alert_config_services: {data: {rules: $rules, type: mail, enabled: true}, on_conflict: {constraint: alert_config_service_pkey, update_columns: rules}}}, on_conflict: {constraint: alert_config_pkey, update_columns: enabled}) {
    affected_rows
  }
} 
`);

export const ADD_SCHEMA_TAG = gql(`
mutation AddSchemaTag($tagName: String!, $projectId: uuid!, $entryHash: String!, $color: String) {
  insert_schema_registry_tags_one(object: {name: $tagName, project_id: $projectId, entry_hash: $entryHash, color: $color}) {
    color
    id
    name
  }
}
`);

export const DELETE_SCHEMA_TAG = gql(`
mutation DeleteSchemaTag($ID: uuid!) {
  delete_schema_registry_tags_by_pk(id: $ID) {
    id
  }
}
`);

export const DELETE_SLACK_APP = gql(`
mutation DeleteSlackApp($projectID: uuid!) {
  deleteSlackApp(args: {projectID: $projectID}) {
    status
  }
}
`);

export const FETCH_SLACK_STATE = gql(`
query FetchSlackState($projectId: uuid!) {
  slack_config(where: {project_id: {_eq: $projectId}}) {
    channel_name
    webhook_url
    project_id
    channel_id
    slack_team_id
    team_name
  }
}
`);

export const SLACK_TOKEN_EXCHANGE_QUERY = gql(`
mutation slackTokenExchange (
  $code: String!
  $projectId: uuid!
  ) {
    slackExchangeOAuthToken (
      code: $code
      projectId: $projectId
    ) {
      channel_name
    }
  }
`);
