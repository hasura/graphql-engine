import { GraphQLError } from 'graphql';

export type SafeSchemaChange = 'NON_BREAKING';
export type DangerousSchemaChange = 'DANGEROUS';
export type BreakingSchemaChange = 'BREAKING';
export type TotalSchemaChanges = 'TOTAL';

export type ChangeLevel =
  | DangerousSchemaChange
  | BreakingSchemaChange
  | SafeSchemaChange
  | TotalSchemaChanges;

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

export type Role = {
  role: string;
  id: string;
};

export type Schema = {
  id: string;
  hash: string;
  entry_hash: string;
  created_at: string;
  roleBasedSchemas: RoleBasedSchema[];
  tags: SchemaRegistryTag[];
};
export type SchemaChangeCard = {
  id: string;
  hash: string;
  entry_hash: string;
  created_at: string;
  roles: Role[];
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
  schema_registry_dumps_aggregate: SchemaRegistryDumpsAggregate;
};
export type GetSchemaChangeListResponseWithError = {
  data?: GetSchemaChangeListQueryResponse;
  errors?: GraphQLError[];
};
export type GetSchemaChangeListQueryResponse = {
  schema_registry_dumps: SchemaChangeListDumpWithSiblingSchema[];
  schema_registry_dumps_aggregate: SchemaRegistryDumpsAggregate;
};

/**
 * Query types for dumps_v2 aggregate
 * START
 */
export type SchemaRegistryDumpsAggregate = {
  aggregate: SchemaRegistryCountAggregate;
};

export type SchemaRegistryCountAggregate = {
  count: number;
};

export type GetSchemaRegstiryDumpsV2AggregateResponseWithError = {
  data?: GetSchemaRegstiryDumpsV2AggregateResponse;
  errors?: GraphQLError[];
};

export type GetSchemaRegistryNotificationResponseWithError = {
  data?: GetSchemaRegistryNotificationResponse;
  errors?: GraphQLError[];
};

export type GetSchemaRegstiryDumpsV2AggregateResponse = {
  schema_registry_dumps_v2_aggregate: SchemaRegistryDumpsAggregate;
  schema_registry_dumps_v2: SchemaRegistryChangeRecordedAt[];
};
export type GetSchemaRegstiryDumpsV1AggregateResponseWithError = {
  data?: GetSchemaRegstiryDumpsV1AggregateResponse;
  errors?: GraphQLError[];
};
export type GetSchemaRegstiryDumpsV1AggregateResponse = {
  schema_registry_dumps_aggregate: SchemaRegistryDumpsAggregate;
};
export type GetSchemaRegistryNotificationResponse = {
  schema_registry_dumps_v2: SchemaRegsitryNotificationData[];
};
export type SchemaRegsitryNotificationData = SchemaRegistryChangeRecordedAt & {
  diff_with_previous_schema: SchemaDiffData[];
};
export type SchemaRegistryChangeRecordedAt = {
  change_recorded_at: string;
};
/**
 * END
 */

/**
 * Query types for dumps_v2
 * START
 */
export type GetSchemaRegstiryDumpsV2ResponseWithError = {
  data?: GetSchemaRegstiryDumpsV2Response;
  errors?: GraphQLError[];
};
export type GetSchemaRegstiryDumpsV2Response = {
  schema_registry_dumps_v2: SchemaRegistryDumpWithSiblingSchema[];
};
/**
 * END
 */

/**
 * Query types for dumps_v1
 * START
 */
export type GetSchemaRegstiryDumpsV1ResponseWithError = {
  data?: GetSchemaRegstiryDumpsV1Response;
  errors?: GraphQLError[];
};
export type GetSchemaRegstiryDumpsV1Response = {
  schema_registry_dumps: SchemaRegistryDumpWithSiblingSchema[];
};
/**
 * END
 */

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
export type SchemaChangeListDumpWithSiblingSchema = {
  id: string;
  entry_hash: string;
  schema_hash: string;
  change_recorded_at: string;
  schema_tags: SchemaRegistryTag[];
  sibling_schemas: SchemaChangeListSiblingSchema[];
};

export type SchemaChangeListSiblingSchema = {
  id: string;
  hasura_schema_role: string;
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
export type GetURLRegistrySchemaResponseWithError = {
  data?: GetURLSchemaChangeQueryResponse;
  errors?: GraphQLError[];
};

export type GetURLSchemaChangeQueryResponse = {
  schema_registry_dumps: SchemaChangeListDumpWithSiblingSchema[];
  schema_registry_dumps_v2: SchemaChangeListDumpWithSiblingSchema[];
};

export type GetRegistrySchemaQueryResponse = {
  schema_registry_dumps: SiblingSchema[];
  schema_registry_dumps_v2: SiblingSchema[];
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
