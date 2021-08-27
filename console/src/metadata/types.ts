import { Nullable } from './../components/Common/utils/tsUtils';
import { Driver } from '../dataSources';
import { PermissionsType } from '../components/Services/RemoteSchema/Permissions/types';

export type DataSource = {
  name: string;
  url: string | { from_env: string };
  driver: Driver;
  connection_pool_settings?: ConnectionPoolSettings;
  read_replicas?: Omit<SourceConnectionInfo, 'connection_string'>[];
};

// GENERATED

export type PGColumn = string;
export type ComputedFieldName = string;
export type RoleName = string;
export type TriggerName = string;
export type RemoteRelationshipName = string;
export type RemoteSchemaName = string;
export type CollectionName = string;
export type GraphQLName = string;
export type GraphQLType = string;
export type RelationshipName = string;
export type ActionName = string;

/**
 * A String value which supports templating environment variables enclosed in {{ and }}.
 * Template example: https://{{ACTION_API_DOMAIN}}/create-user
 */
export type WebhookURL = string;

// //////////////////////////////
// #region TABLES
// /////////////////////////////

export type TableName = string | QualifiedTable;

export interface QualifiedTable {
  name: string;
  schema: string;
}

export interface QualifiedTableBigQuery {
  name: string;
  dataset: string;
}

/**
 * Configuration for the table/view
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/table-view.html#table-config
 */
export interface TableConfig {
  /** Customise the table name */
  custom_name: string;
  /** Customise the root fields */
  custom_root_fields?: CustomRootFields;
  /** Customise the column names */
  custom_column_names?: CustomColumnNames;
}

/**
 * Representation of a table in metadata, 'tables.yaml' and 'metadata.json'
 */
export interface TableEntry {
  table: QualifiedTable;
  is_enum?: boolean;
  configuration?: TableConfig;
  event_triggers?: EventTrigger[];
  computed_fields?: ComputedField[];
  object_relationships?: ObjectRelationship[];
  array_relationships?: ArrayRelationship[];
  remote_relationships?: RemoteRelationship[];
  insert_permissions?: InsertPermissionEntry[];
  select_permissions?: SelectPermissionEntry[];
  update_permissions?: UpdatePermissionEntry[];
  delete_permissions?: DeletePermissionEntry[];
}

/**
 * Customise the root fields
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/table-view.html#custom-root-fields
 */
export interface CustomRootFields {
  /** Customise the `<table-name>` root field */
  select?: string;
  /** Customise the `<table-name>_by_pk` root field */
  select_by_pk?: string;
  /** Customise the `<table-name>_aggregate` root field */
  select_aggregate?: string;
  /** Customise the `insert_<table-name>` root field */
  insert?: string;
  /** Customise the `insert_<table-name>_one` root field */
  insert_one?: string;
  /** Customise the `update_<table-name>` root field */
  update?: string;
  /** Customise the `update_<table-name>_by_pk` root field */
  update_by_pk?: string;
  /** Customise the `delete_<table-name>` root field */
  delete?: string;
  /** Customise the `delete_<table-name>_by_pk` root field */
  delete_by_pk?: string;
}

/**
 * A JSON Object of Postgres column name to GraphQL name mapping
 */
export interface CustomColumnNames {
  [key: string]: string;
}

// ////////////////////////////
// #endregion TABLES
// /////////////////////////////

// //////////////////////////////
// #region CUSTOM FUNCTIONS
// /////////////////////////////

export type FunctionName = string | QualifiedFunction;

export interface QualifiedFunction {
  name: string;
  schema: string;
}

/**
 * A custom SQL function to add to the GraphQL schema with configuration.
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-functions.html#args-syntax
 */
export interface Function {
  /** Name of the SQL function */
  function: FunctionName;
  /** Configuration for the SQL function */
  configuration?: FunctionConfiguration;
}

export interface FunctionDefinition {
  name: string;
  schema: string;
}

/**
 * Configuration for a CustomFunction
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-functions.html#function-configuration
 */
export interface FunctionConfiguration {
  /**
   * Function argument which accepts session info JSON
   * Currently, only functions which satisfy the following constraints can be exposed over the GraphQL API (terminology from Postgres docs):
   * - Function behaviour: ONLY `STABLE` or `IMMUTABLE`
   * - Return type: MUST be `SETOF <table-name>`
   * - Argument modes: ONLY `IN`
   */
  session_argument?: string;
}

export interface FunctionPermission {
  role: string;
  definition?: Record<string, any>;
}

// ////////////////////////////
// #endregion CUSTOM FUNCTIONS
// /////////////////////////////

// //////////////////////////////
// #region RELATIONSHIPS
// /////////////////////////////

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#args-syntax
 */
export interface ObjectRelationship {
  /** Name of the new relationship */
  name: string;
  /** Use one of the available ways to define an object relationship */
  using: ObjRelUsing;
  /** Comment */
  comment?: string;
}

/**
 * Use one of the available ways to define an object relationship
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#objrelusing
 */
export interface ObjRelUsing {
  /** The column with foreign key constraint */
  foreign_key_constraint_on?: PGColumn;
  /** Manual mapping of table and columns */
  manual_configuration?: ObjRelUsingManualMapping;
}

/**
 * Manual mapping of table and columns
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#objrelusingmanualmapping
 */
export interface ObjRelUsingManualMapping {
  /** The table to which the relationship has to be established */
  remote_table: TableName;
  /** Mapping of columns from current table to remote table */
  column_mapping: { [key: string]: string };
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#create-array-relationship-syntax
 */
export interface ArrayRelationship {
  /** Name of the new relationship */
  name: string;
  /** Use one of the available ways to define an array relationship */
  using: ArrRelUsing;
  /** Comment */
  comment?: string;
}

/**
 * Use one of the available ways to define an object relationship
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#arrrelusing
 */
export interface ArrRelUsing {
  /** The column with foreign key constraint */
  foreign_key_constraint_on?: ArrRelUsingFKeyOn;
  /** Manual mapping of table and columns */
  manual_configuration?: ArrRelUsingManualMapping;
}

/**
 * The column with foreign key constraint
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#arrrelusingfkeyon
 */
export interface ArrRelUsingFKeyOn {
  column: PGColumn;
  table: TableName;
}

/**
 * Manual mapping of table and columns
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#arrrelusingmanualmapping
 */
export interface ArrRelUsingManualMapping {
  /** The table to which the relationship has to be established */
  remote_table: TableName;
  /** Mapping of columns from current table to remote table */
  column_mapping: { [key: string]: string };
}

/**
 * Preset values for columns that can be sourced from session variables or static values.
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#columnpresetexp
 */
export interface ColumnPresetsExpression {
  [key: string]: string;
}

// //////////////////////////////
// #endregion RELATIONSHIPS
// /////////////////////////////

// //////////////////////////////
// #region PERMISSIONS
// /////////////////////////////

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#args-syntax
 */
export interface InsertPermissionEntry {
  /** Role */
  role: RoleName;
  /** The permission definition */
  permission: InsertPermission;
  /** Comment */
  comment?: string;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#insertpermission
 */
export interface InsertPermission {
  /** This expression has to hold true for every new row that is inserted */
  check?: { [key: string]: Record<string, any> | string | number };
  /** Preset values for columns that can be sourced from session variables or static values */
  set?: ColumnPresetsExpression;
  /** Can insert into only these columns (or all when '*' is specified) */
  columns: PGColumn[] | '*';
  /**
   * When set to true the mutation is accessible only if x-hasura-use-backend-only-permissions session variable exists
   * and is set to true and request is made with x-hasura-admin-secret set if any auth is configured
   */
  backend_only?: boolean;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#create-select-permission-syntax
 */
export interface SelectPermissionEntry {
  /** Role */
  role: RoleName;
  /** The permission definition */
  permission: SelectPermission;
  /** Comment */
  comment?: string;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#selectpermission
 */
export interface SelectPermission {
  /** Only these columns are selectable (or all when '*' is specified) */
  columns: PGColumn[] | '*';
  /** Only these computed fields are selectable */
  computed_fields?: ComputedFieldName[];
  /**
   * The maximum number of rows that can be returned
   * @TJS-type integer
   */
  limit?: number;
  /** Toggle allowing aggregate queries */
  allow_aggregations?: boolean;
  /** Only the rows where this precondition holds true are selectable */
  filter?: { [key: string]: Record<string, any> | string | number };
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#create-update-permission-syntax
 */
export interface UpdatePermissionEntry {
  /** Role */
  role: RoleName;
  /** The permission definition */
  permission: UpdatePermission;
  /** Comment */
  comment?: string;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#updatepermission
 */
export interface UpdatePermission {
  /** Postcondition which must be satisfied by rows which have been updated */
  check?: { [key: string]: Record<string, any> | string | number };
  /** Preset values for columns that can be sourced from session variables or static values */
  set?: ColumnPresetsExpression;
  /** Only these columns are selectable (or all when '*' is specified) */
  columns: PGColumn[] | '*';
  /** Only the rows where this precondition holds true are updatable */
  filter?: { [key: string]: Record<string, any> | string | number };
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#create-delete-permission-syntax
 */
export interface DeletePermissionEntry {
  /** Role */
  role: RoleName;
  /** The permission definition */
  permission: DeletePermission;
  /** Comment */
  comment?: string;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#deletepermission
 */
export interface DeletePermission {
  /** Only the rows where this precondition holds true are updatable */
  filter?: { [key: string]: Record<string, any> | string | number };
}

// //////////////////////////////
//  #endregion PERMISSIONS
// /////////////////////////////

// //////////////////////////////
// #region COMPUTED FIELDS
// /////////////////////////////

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/computed-field.html#args-syntax
 */
export interface ComputedField {
  /** Name of the new computed field */
  name: ComputedFieldName;
  /** The computed field definition */
  definition: ComputedFieldDefinition;
  /** Comment */
  comment?: string;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/computed-field.html#computedfielddefinition
 */
export interface ComputedFieldDefinition {
  /** The SQL function */
  function: FunctionName;
  /** Name of the argument which accepts a table row type. If omitted, the first argument is considered a table argument */
  table_argument?: string;
  /** Name of the argument which accepts the Hasura session object as a JSON/JSONB value. If omitted, the Hasura session object is not passed to the function */
  session_argument?: string;
}

// //////////////////////////////
//  #endregion COMPUTED FIELDS
// /////////////////////////////

// //////////////////////////////
// #region EVENT TRIGGERS
// /////////////////////////////

/**
 * NOTE: The metadata type doesn't QUITE match the 'create' arguments here
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#create-event-trigger
 */
export interface EventTrigger {
  /** Name of the event trigger */
  name: TriggerName;
  /** The SQL function */
  definition: EventTriggerDefinition;
  /** The SQL function */
  retry_conf: RetryConf;
  /** The SQL function */
  webhook?: string;
  webhook_from_env?: string;
  /** The SQL function */
  headers?: ServerHeader[];
}

export interface EventTriggerDefinition {
  enable_manual: boolean;
  insert?: OperationSpec;
  delete?: OperationSpec;
  update?: OperationSpec;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#eventtriggercolumns
 */
export type EventTriggerColumns = '*' | PGColumn[];

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#operationspec
 */
export interface OperationSpec {
  columns: EventTriggerColumns;
  payload?: EventTriggerColumns;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#headerfromvalue
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#headerfromenv
 */
export interface ServerHeader {
  /** Name of the header */
  name: string;
  /** Value of the header */
  value?: string;
  /** value obtained from env file */
  value_from_env?: string;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#retryconf
 */
export interface RetryConf {
  /**
   * Number of times to retry delivery.
   * Default: 0
   * @TJS-type integer
   */
  num_retries?: number;
  /**
   * Number of seconds to wait between each retry.
   * Default: 10
   * @TJS-type integer
   */
  interval_sec?: number;
  /**
   * Number of seconds to wait for response before timing out.
   * Default: 60
   * @TJS-type integer
   */
  timeout_sec?: number;
}

// //////////////////////////////
//  #endregion EVENT TRIGGERS
// /////////////////////////////

// //////////////////////////////
// #region SCHEDULED TRIGGERS
// /////////////////////////////

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/scheduled-triggers.html#create-cron-trigger
 */
export interface CronTrigger {
  /**	Name of the cron trigger */
  name: TriggerName;
  /**	URL of the webhook */
  webhook: WebhookURL;
  /**	Cron expression at which the trigger should be invoked. */
  schedule: string;
  /** Any JSON payload which will be sent when the webhook is invoked. */
  payload?: Record<string, any>;
  /** List of headers to be sent with the webhook */
  headers: ServerHeader[];
  /**	Retry configuration if scheduled invocation delivery fails */
  retry_conf?: RetryConfST;
  /**	Flag to indicate whether a trigger should be included in the metadata. When a cron trigger is included in the metadata, the user will be able to export it when the metadata of the graphql-engine is exported. */
  include_in_metadata: boolean;
  /**	Custom comment. */
  comment?: string;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/scheduled-triggers.html#retryconfst
 */
export interface RetryConfST {
  /**
   * Number of times to retry delivery.
   * Default: 0
   * @TJS-type integer
   */
  num_retries?: number;
  /**
   * Number of seconds to wait between each retry.
   * Default: 10
   * @TJS-type integer
   */
  retry_interval_seconds?: number;
  /**
   * Number of seconds to wait for response before timing out.
   * Default: 60
   * @TJS-type integer
   */
  timeout_seconds?: number;
  /**
   * Number of seconds between scheduled time and actual delivery time that is acceptable. If the time difference is more than this, then the event is dropped.
   * Default: 21600 (6 hours)
   * @TJS-type integer
   */
  tolerance_seconds?: number;
}

// //////////////////////////////
//  #endregion SCHEDULED TRIGGERS
// /////////////////////////////

// //////////////////////////////
// #region REMOTE SCHEMAS
// /////////////////////////////

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/remote-schemas.html#add-remote-schema
 */
export interface RemoteSchema {
  /** Name of the remote schema */
  name: string;
  /** Name of the remote schema */
  definition: RemoteSchemaDef;
  /** Comment */
  comment?: string;
  permissions?: PermissionsType[];
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#remoteschemadef
 */
export interface RemoteSchemaDef {
  url?: string;
  url_from_env?: string;
  headers?: ServerHeader[];
  forward_client_headers?: boolean;
  timeout_seconds?: number;
}

// //////////////////////////////
//  #endregion REMOTE SCHEMAS
// /////////////////////////////

// //////////////////////////////
// #region REMOTE RELATIONSHIPS
// /////////////////////////////

// NOTE: RemoteRelationship Metadata shape is slightly different than 'create' arguments here

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/remote-relationships.html#args-syntax
 */
export interface RemoteRelationship {
  /** Name of the remote relationship */
  name: RemoteRelationshipName;
  /** Definition object */
  definition: RemoteRelationshipDef;
}

export interface RemoteRelationshipDef {
  /**
   * Column(s) in the table that is used for joining with remote schema field.
   * All join keys in remote_field must appear here.
   */
  hasura_fields: PGColumn[];
  /** Name of the remote schema to join with */
  remote_schema: RemoteSchemaName;
  /** The schema tree ending at the field in remote schema which needs to be joined with. */
  remote_field: RemoteField;
}

/**
 * A recursive tree structure that points to the field in the remote schema that needs to be joined with.
 * It is recursive because the remote field maybe nested deeply in the remote schema.
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/remote-relationships.html#remotefield
 */
export interface RemoteField {
  [FieldName: string]: {
    arguments: InputArguments;
    field?: RemoteField;
  };
}

/**
 * Note: Table columns can be referred by prefixing $ e.g $id.
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/remote-relationships.html#inputarguments
 */
export interface InputArguments {
  [InputField: string]: PGColumn;
}

// //////////////////////////////
// #endregion REMOTE RELATIONSHIPS
// /////////////////////////////

// //////////////////////////////
// #region QUERY COLLECTIONS
// /////////////////////////////

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/query-collections.html#args-syntax
 */
export interface QueryCollectionEntry {
  /** Name of the query collection */
  name: CollectionName;
  /** List of queries */
  definition: {
    queries: QueryCollection[];
  };
  /** Comment */
  comment?: string;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#collectionquery
 */
export interface QueryCollection {
  name: string;
  query: string;
}

// //////////////////////////////
// #endregion QUERY COLLECTIONS
// /////////////////////////////

// //////////////////////////////
// #region ALLOW LIST
// /////////////////////////////

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/query-collections.html#add-collection-to-allowlist-syntax
 */
export interface AllowList {
  /** Name of a query collection to be added to the allow-list */
  collection: CollectionName;
}

// //////////////////////////////
//  #endregion ALLOW LIST
// /////////////////////////////

// //////////////////////////////
// #region CUSTOM TYPES
// /////////////////////////////

export interface CustomTypes {
  input_objects?: InputObjectType[];
  objects?: ObjectType[];
  scalars?: ScalarType[];
  enums?: EnumType[];
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#inputobjecttype
 */
export interface InputObjectType {
  /** Name of the Input object type */
  name: GraphQLName;
  /** Description of the Input object type */
  description?: string;
  /** Fields of the Input object type */
  fields: InputObjectField[];
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#inputobjectfield
 */
export interface InputObjectField {
  /** Name of the Input object type */
  name: GraphQLName;
  /** Description of the Input object type */
  description?: string;
  /** GraphQL type of the Input object type */
  type: GraphQLType;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#objecttype
 */
export interface ObjectType {
  /** Name of the Input object type */
  name: GraphQLName;
  /** Description of the Input object type */
  description?: string;
  /** Fields of the Input object type */
  fields: InputObjectField[];
  /** Relationships of the Object type to tables */
  relationships?: CustomTypeObjectRelationship[];
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#objectfield
 */
export interface ObjectField {
  /** Name of the Input object type */
  name: GraphQLName;
  /** Description of the Input object type */
  description?: string;
  /** GraphQL type of the Input object type */
  type: GraphQLType;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#objectrelationship
 */
export interface CustomTypeObjectRelationship {
  /** Name of the relationship, shouldn’t conflict with existing field names */
  name: RelationshipName;
  /** Type of the relationship */
  type: 'object' | 'array';
  /** The table to which relationship is defined */
  remote_table: TableName;
  /** Mapping of fields of object type to columns of remote table  */
  field_mapping: {
    [ObjectFieldName: string]: string;
  };
  /** Source name, where remote_table exists */
  source?: string;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#scalartype
 */
export interface ScalarType {
  /** Name of the Scalar type */
  name: GraphQLName;
  /** Description of the Scalar type */
  description?: string;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#enumtype
 */
export interface EnumType {
  /** Name of the Enum type */
  name: GraphQLName;
  /** Description of the Enum type */
  description?: string;
  /** Values of the Enum type */
  values: EnumValue[];
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#enumvalue
 */
export interface EnumValue {
  /** Value of the Enum type */
  value: GraphQLName;
  /** Description of the Enum value */
  description?: string;
  /** If set to true, the enum value is marked as deprecated */
  is_deprecated?: boolean;
}

// //////////////////////////////
//  #endregion CUSTOM TYPES
// /////////////////////////////

// //////////////////////////////
// #region ACTIONS
// /////////////////////////////

// Note: Action Metadata type is slightly different than "create" type

/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/actions.html#args-syntax
 */
export interface Action {
  /** Name of the action  */
  name: ActionName;
  /** Definition of the action */
  definition: ActionDefinition;
  /** Comment */
  comment?: string;
  /** Permissions of the action */
  permissions?: Array<{ role: string }>;
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/actions.html#actiondefinition
 */
export interface ActionDefinition {
  arguments?: InputArgument[];
  output_type?: string;
  kind?: 'synchronous' | 'asynchronous';
  headers?: ServerHeader[];
  forward_client_headers?: boolean;
  handler: WebhookURL;
  type?: 'mutation' | 'query';
}

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/actions.html#inputargument
 */
export interface InputArgument {
  name: string;
  type: GraphQLType;
}

// //////////////////////////////
//  #endregion ACTIONS
// /////////////////////////////

// /////////////////////////////
// #region REST ENDPOINT
// /////////////////////////////

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api//schema-metadata-api/restified-endpoints.html
 */

export type AllowedRESTMethods = 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE';

export interface RestEndpointDefinition {
  query: {
    query_name: string;
    collection_name: string;
  };
}

export interface BigQueryServiceAccount {
  project_id?: string;
  client_email?: string;
  private_key?: string;
  from_env?: string;
}

export interface RestEndpointEntry {
  name: string;
  url: string;
  methods: AllowedRESTMethods[];
  definition: RestEndpointDefinition;
  comment?: string;
}

// //////////////////////////////
//  #endregion REST ENDPOINT
// /////////////////////////////

/**
 * Docs for type: https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgsourceconnectioninfo
 */

export type SSLModeOptions = 'verify-ca' | 'verify-full' | 'disable';

export type IsolationLevelOptions =
  | 'read-committed'
  | 'repeatable-read'
  | 'serializable';

export interface SSLConfigOptions {
  sslmode?: SSLModeOptions;
  sslrootcert?: {
    from_env: string;
  };
  sslcert?: {
    from_env: string;
  };
  sslkey?: {
    from_env: string;
  };
  sslpassword?: {
    from_env: string;
  };
}

export interface ConnectionPoolSettings {
  max_connections?: number;
  idle_timeout?: number;
  retries?: number;
  pool_timeout?: number;
  connection_lifetime?: number;
}

export interface SourceConnectionInfo {
  // used for SQL Server
  connection_string?: string | { from_env: string };
  // used for Postgres
  database_url?: string | { from_env: string };
  use_prepared_statements?: boolean;
  isolation_level?: IsolationLevelOptions;
  pool_settings?: ConnectionPoolSettings;
  ssl_configuration?: SSLConfigOptions;
}

/**
 * Type used in exported 'metadata.json' and replace metadata endpoint
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/manage-metadata.html#replace-metadata
 */
export interface HasuraMetadataV2 {
  version: '2';
  tables: TableEntry[];
  actions?: Action[];
  custom_types?: CustomTypes;
  functions?: Array<{
    function: { schema: string; name: string };
    configuration: Record<string, any>;
  }>;
  remote_schemas?: RemoteSchema[];
  query_collections?: QueryCollectionEntry[];
  allowlist?: AllowList[];
  cron_triggers?: CronTrigger[];
}

export interface MetadataDataSource {
  name: string;
  kind?: 'postgres' | 'mysql' | 'mssql' | 'bigquery' | 'citus';
  configuration?: {
    connection_info?: SourceConnectionInfo;
    // pro-only feature
    read_replicas?: SourceConnectionInfo[];
    service_account?: BigQueryServiceAccount;
    global_select_limit?: number;
    project_id?: string;
    datasets?: string[];
  };
  tables: TableEntry[];
  functions?: Array<{
    function: QualifiedFunction;
    configuration?: {
      exposed_as?: 'mutation' | 'query';
      session_argument?: string;
    };
    permissions?: FunctionPermission[];
  }>;
  query_collections?: QueryCollectionEntry[];
  allowlist?: AllowList[];
}

export interface InheritedRole {
  role_name: string;
  role_set: string[];
}

export interface DomainList {
  host: string;
  suffix?: string;
  perms?: string[];
}
export interface APILimits {
  per_role?: Record<string, number>;
  global?: number;
}

type APILimit<T> = {
  global: T;
  per_role?: Record<string, T>;
};

export interface HasuraMetadataV3 {
  version: 3;
  sources: MetadataDataSource[];
  remote_schemas?: RemoteSchema[];
  actions?: Action[];
  custom_types?: CustomTypes;
  cron_triggers?: CronTrigger[];
  query_collections?: QueryCollectionEntry[];
  allowlist?: AllowList[];
  inherited_roles: InheritedRole[];
  network?: { tls_allowlist?: DomainList[] };
  rest_endpoints?: RestEndpointEntry[];
  api_limits?: {
    disabled?: boolean;
    depth_limit?: APILimit<number>;
    node_limit?: APILimit<number>;
    rate_limit?: APILimit<{
      unique_params: Nullable<'IP' | string[]>;
      max_reqs_per_min: number;
    }>;
  };
  graphql_schema_introspection?: {
    disabled_for_roles: string[];
  };
}

// Inconsistent Objects

export interface InconsistentObject {
  definition:
    | string
    | {
        comment: string;
        definition: InconsistentObjectDefinition;
      };
  reason: string;
  name: string;
  type: string;
  message:
    | string
    | {
        message: string;
        request: InconsistentObjectRequest;
      };
}

type InconsistentObjectRequest = {
  proxy: string | null;
  secure: boolean;
  path: string;
  responseTimeout: string;
  method: 'POST' | 'GET' | 'PUT' | 'DELETE' | 'PATCH' | 'OPTION';
  host: string;
  requestVersion: `${number}`;
  redirectCount: `${number}`;
  port: `${number}`;
};

type InconsistentObjectDefinition = {
  timeout_seconds: number;
  url_from_env: string;
  forward_client_headers: boolean;
};
