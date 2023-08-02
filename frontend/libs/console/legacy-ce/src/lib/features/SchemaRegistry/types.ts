import { GraphQLError } from 'graphql';

export type SafeSchemaChange = 'NON_BREAKING';
export type DangerousSchemaChange = 'DANGEROUS';
export type BreakingSchemaChange = 'BREAKING';

export type ChangeLevel =
  | DangerousSchemaChange
  | BreakingSchemaChange
  | SafeSchemaChange;

export type SchemaChange = {
  criticality: {
    level: ChangeLevel;
    reason?: string;
  };
  type: string;
  meta: any;
  message: string;
  path: string;
};

export type RoleBasedSchema = {
  raw: string;
  role: string;
  entry_hash: string;
  hash: string;
  id: string;
  changes?: SchemaChange[];
};

export type Schema = {
  id: string;
  hash: string;
  entry_hash: string;
  created_at: string;
  roleBasedSchemas: RoleBasedSchema[];
  tags: SchemaRegistryTag[];
};

/*
  Schema Registry Query types
*/

export type GetSchemaListResponseWithError = {
  data?: GetSchemaListQueryResponse;
  errors?: GraphQLError[];
};

export type GetSchemaListQueryResponse = {
  schema_registry_dumps: SchemaRegistryDumpWithSiblingSchema[];
};

export type SchemaRegistryDump = {
  id: string;
  entry_hash: string;
  schema_hash: string;
  change_recorded_at: string;
  created_at: string;
  hasura_schema_role: string;
  schema_sdl: string;
  schema_tags: SchemaRegistryTag[];
};

export type SchemaRegistryDumpWithSiblingSchema = SchemaRegistryDump & {
  sibling_schemas: SiblingSchema[];
};

export type SiblingSchema = SchemaRegistryDump & {
  diff_with_previous_schema: SchemaDiffData[];
};

export type SchemaDiffData = {
  current_schema_hash: string;
  former_schema_hash: string;
  former_schema_id: string;
  current_schema_id: string;
  // For the following, using the same type (SchemaChange) used for UI, if ever the query response type changes
  // a refactor in UI will also be required thus changing the inherent type
  schema_diff_data: SchemaChange[];
};

export type GetRegistrySchemaResponseWithError = {
  data?: GetRegistrySchemaQueryResponse;
  errors?: GraphQLError[];
};

export type GetRegistrySchemaQueryResponse = {
  schema_registry_dumps: SiblingSchema[];
};

export type GetAlertConfigResponseWithError = {
  data?: GetAlertConfigQueryResponse;
  errors?: GraphQLError[];
};

export type GetAlertConfigQueryResponse = {
  alert_config_service: AlertConfig[];
};

export type AlertConfig = {
  project_id: string;
  type: AlertType;
  metadata: Record<string, any>;
  rules: Record<string, boolean>;
};

export type SetAlertConfigResponseWithError = {
  data?: SetAlertConfigQueryResponse;
  errors?: GraphQLError[];
};

export type SchemaRegistryAlert = {
  id: string;
  project_id: string;
  alert_type: string;
  config: Record<string, boolean>;
  slack_webhook: string;
  meta: Record<string, any>;
};

export type SetAlertConfigQueryResponse = {
  schema_registry_alerts: SchemaRegistryAlert;
};

export type SetSchemaRegistryAlert = {
  id: string;
  project_id: string;
  alert_type: string;
  config: Record<string, boolean>;
};

export type ConfigKey = 'safe' | 'dangerous' | 'breaking';

// Tag types
export type Tag = {
  id: string;
  name: string;
  color: string;
  projectID?: string;
};

export type CreateSchemaRegistryTagResponseWithError = {
  data?: CreateSchemaRegistryTagMutationInsertResponse;
  errors?: GraphQLError[];
};

export type CreateSchemaRegistryTagMutationInsertResponse = {
  insert_schema_registry_tags_one: SchemaRegistryTag;
};

export type SchemaRegistryTag = {
  id: string;
  name: string;
  color: string;
  entryHash?: string;
};

export type DeleteSchemaRegistryTagResponseWithError = {
  data?: DeleteSchemaRegistryTagMutationResponse;
  errors?: GraphQLError[];
};

export type DeleteSchemaRegistryTagMutationResponse = {
  delete_schema_registry_tags_by_pk: DeletedTagID;
};

export type DeletedTagID = {
  id: string;
};

type MailAlertType = 'mail';
type SlackAlertType = 'slack';

export type AlertType = MailAlertType | SlackAlertType;

export type DeleteSlackAppMutationResponseWithError = {
  data?: DeleteSlackAppMutationResponse;
  errors?: GraphQLError[];
};

export type DeleteSlackAppMutationResponse = {
  deleteSlackApp: {
    status: string;
  };
};

export type GetSlackStateResponseWithError = {
  data?: GetSlackStateQueryResponse;
  errors?: GraphQLError[];
};

export type GetSlackStateQueryResponse = {
  slack_config: SlackConfig[];
};

export type SlackConfig = {
  channel_name: string;
  webhook_url: string;
  project_id: string;
  channel_id: string;
  slack_team_id: string;
  team_name: string;
};
