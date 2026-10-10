/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#headerfromvalue
 */
export interface HeaderFromValue {
  /**
   * Name of the header
   */
  name: string;
  /**
   * Value of the header
   */
  value: string;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#headerfromenv
 */
export interface HeaderFromEnv {
  /**
   * Name of the header
   */
  name: string;
  /**
   * Name of the environment variable which holds the value of the header
   */
  value_from_env: string;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#objectfield
 */
export interface ObjectField {
  /**
   * Description of the Input object type
   */
  description?: string;
  /**
   * Name of the Input object type
   */
  name: string;
  /**
   * GraphQL type of the Input object type
   */
  type: string;
}
/**
 * Type used in exported 'metadata.json' and replace metadata endpoint
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/manage-metadata.html#replace-metadata
 */
export interface HasuraMetadataV2 {
  actions?: Action[];
  allowlist?: AllowList[];
  cron_triggers?: CronTrigger[];
  custom_types?: CustomTypes;
  functions?: CustomFunction[];
  query_collections?: QueryCollectionEntry[];
  remote_schemas?: RemoteSchema[];
  tables: TableEntry[];
  version: number;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/actions.html#args-syntax
 */
export interface Action {
  /**
   * Comment
   */
  comment?: string;
  /**
   * Definition of the action
   */
  definition: ActionDefinition;
  /**
   * Name of the action
   */
  name: string;
  /**
   * Permissions of the action
   */
  permissions?: Permissions;
}
/**
 * Definition of the action
 *
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/actions.html#actiondefinition
 */
export interface ActionDefinition {
  arguments?: InputArgument[];
  forward_client_headers?: boolean;
  /**
   * A String value which supports templating environment variables enclosed in {{ and }}.
   * Template example: https://{{ACTION_API_DOMAIN}}/create-user
   */
  handler: string;
  headers?: Header[];
  kind?: string;
  output_type?: string;
  type?: ActionDefinitionType;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/actions.html#inputargument
 */
export interface InputArgument {
  name: string;
  type: string;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#headerfromvalue
 *
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#headerfromenv
 */
export interface Header {
  /**
   * Name of the header
   */
  name: string;
  /**
   * Value of the header
   */
  value?: string;
  /**
   * Name of the environment variable which holds the value of the header
   */
  value_from_env?: string;
}
export declare enum ActionDefinitionType {
  Mutation = 'mutation',
  Query = 'query',
}
/**
 * Permissions of the action
 */
export interface Permissions {
  role: string;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/query-collections.html#add-collection-to-allowlist-syntax
 */
export interface AllowList {
  /**
   * Name of a query collection to be added to the allow-list
   */
  collection: string;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/scheduled-triggers.html#create-cron-trigger
 */
export interface CronTrigger {
  /**
   * Custom comment.
   */
  comment?: string;
  /**
   * List of headers to be sent with the webhook
   */
  headers: Header[];
  /**
   * Flag to indicate whether a trigger should be included in the metadata. When a cron
   * trigger is included in the metadata, the user will be able to export it when the metadata
   * of the graphql-engine is exported.
   */
  include_in_metadata: boolean;
  /**
   * Name of the cron trigger
   */
  name: string;
  /**
   * Any JSON payload which will be sent when the webhook is invoked.
   */
  payload?: {
    [key: string]: any;
  };
  /**
   * Retry configuration if scheduled invocation delivery fails
   */
  retry_conf?: RetryConfST;
  /**
   * Cron expression at which the trigger should be invoked.
   */
  schedule: string;
  /**
   * URL of the webhook
   */
  webhook: string;
}
/**
 * Retry configuration if scheduled invocation delivery fails
 *
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/scheduled-triggers.html#retryconfst
 */
export interface RetryConfST {
  /**
   * Number of times to retry delivery.
   * Default: 0
   */
  num_retries?: number;
  /**
   * Number of seconds to wait between each retry.
   * Default: 10
   */
  retry_interval_seconds?: number;
  /**
   * Number of seconds to wait for response before timing out.
   * Default: 60
   */
  timeout_seconds?: number;
  /**
   * Number of seconds between scheduled time and actual delivery time that is acceptable. If
   * the time difference is more than this, then the event is dropped.
   * Default: 21600 (6 hours)
   */
  tolerance_seconds?: number;
}
export interface CustomTypes {
  enums?: EnumType[];
  input_objects?: InputObjectType[];
  objects?: ObjectType[];
  scalars?: ScalarType[];
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#enumtype
 */
export interface EnumType {
  /**
   * Description of the Enum type
   */
  description?: string;
  /**
   * Name of the Enum type
   */
  name: string;
  /**
   * Values of the Enum type
   */
  values: EnumValue[];
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#enumvalue
 */
export interface EnumValue {
  /**
   * Description of the Enum value
   */
  description?: string;
  /**
   * If set to true, the enum value is marked as deprecated
   */
  is_deprecated?: boolean;
  /**
   * Value of the Enum type
   */
  value: string;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#inputobjecttype
 */
export interface InputObjectType {
  /**
   * Description of the Input object type
   */
  description?: string;
  /**
   * Fields of the Input object type
   */
  fields: InputObjectField[];
  /**
   * Name of the Input object type
   */
  name: string;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#inputobjectfield
 */
export interface InputObjectField {
  /**
   * Description of the Input object type
   */
  description?: string;
  /**
   * Name of the Input object type
   */
  name: string;
  /**
   * GraphQL type of the Input object type
   */
  type: string;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#objecttype
 */
export interface ObjectType {
  /**
   * Description of the Input object type
   */
  description?: string;
  /**
   * Fields of the Input object type
   */
  fields: InputObjectField[];
  /**
   * Name of the Input object type
   */
  name: string;
  /**
   * Relationships of the Object type to tables
   */
  relationships?: CustomTypeObjectRelationship[];
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#objectrelationship
 */
export interface CustomTypeObjectRelationship {
  /**
   * Mapping of fields of object type to columns of remote table
   */
  field_mapping: {
    [key: string]: string;
  };
  /**
   * Name of the relationship, shouldn’t conflict with existing field names
   */
  name: string;
  /**
   * The table to which relationship is defined
   */
  remote_table: QualifiedTable | string;
  /**
   * Type of the relationship
   */
  type: CustomTypeObjectRelationshipType;
}
export interface QualifiedTable {
  name: string;
  schema: string;
}
/**
 * Type of the relationship
 */
export declare enum CustomTypeObjectRelationshipType {
  Array = 'array',
  Object = 'object',
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-types.html#scalartype
 */
export interface ScalarType {
  /**
   * Description of the Scalar type
   */
  description?: string;
  /**
   * Name of the Scalar type
   */
  name: string;
}
/**
 * A custom SQL function to add to the GraphQL schema with configuration.
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-functions.html#args-syntax
 */
export interface CustomFunction {
  /**
   * Configuration for the SQL function
   */
  configuration?: FunctionConfiguration;
  /**
   * Name of the SQL function
   */
  function: QualifiedFunction | string;
}
/**
 * Configuration for the SQL function
 *
 * Configuration for a CustomFunction
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/custom-functions.html#function-configuration
 */
export interface FunctionConfiguration {
  /**
   * Function argument which accepts session info JSON
   * Currently, only functions which satisfy the following constraints can be exposed over the
   * GraphQL API (terminology from Postgres docs):
   * - Function behaviour: ONLY `STABLE` or `IMMUTABLE`
   * - Return type: MUST be `SETOF <table-name>`
   * - Argument modes: ONLY `IN`
   */
  session_argument?: string;
}
export interface QualifiedFunction {
  name: string;
  schema: string;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/query-collections.html#args-syntax
 */
export interface QueryCollectionEntry {
  /**
   * Comment
   */
  comment?: string;
  /**
   * List of queries
   */
  definition: Definition;
  /**
   * Name of the query collection
   */
  name: string;
}
/**
 * List of queries
 */
export interface Definition {
  queries: QueryCollection[];
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#collectionquery
 */
export interface QueryCollection {
  name: string;
  query: string;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/remote-schemas.html#add-remote-schema
 */
export interface RemoteSchema {
  /**
   * Comment
   */
  comment?: string;
  /**
   * Name of the remote schema
   */
  definition: RemoteSchemaDef;
  /**
   * Name of the remote schema
   */
  name: string;
}
/**
 * Name of the remote schema
 *
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/syntax-defs.html#remoteschemadef
 */
export interface RemoteSchemaDef {
  forward_client_headers?: boolean;
  headers?: Header[];
  timeout_seconds?: number;
  url?: string;
  url_from_env?: string;
}
/**
 * Representation of a table in metadata, 'tables.yaml' and 'metadata.json'
 */
export interface TableEntry {
  array_relationships?: ArrayRelationship[];
  computed_fields?: ComputedField[];
  /**
   * Configuration for the table/view
   *
   * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/table-view.html#table-config
   */
  configuration?: TableConfig;
  delete_permissions?: DeletePermissionEntry[];
  event_triggers?: EventTrigger[];
  insert_permissions?: InsertPermissionEntry[];
  is_enum?: boolean;
  object_relationships?: ObjectRelationship[];
  remote_relationships?: RemoteRelationship[];
  select_permissions?: SelectPermissionEntry[];
  table: QualifiedTable;
  update_permissions?: UpdatePermissionEntry[];
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#create-array-relationship-syntax
 */
export interface ArrayRelationship {
  /**
   * Comment
   */
  comment?: string;
  /**
   * Name of the new relationship
   */
  name: string;
  /**
   * Use one of the available ways to define an array relationship
   */
  using: ArrRelUsing;
}
/**
 * Use one of the available ways to define an array relationship
 *
 * Use one of the available ways to define an object relationship
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#arrrelusing
 */
export interface ArrRelUsing {
  /**
   * The column with foreign key constraint
   */
  foreign_key_constraint_on?: ArrRelUsingFKeyOn;
  /**
   * Manual mapping of table and columns
   */
  manual_configuration?: ArrRelUsingManualMapping;
}
/**
 * The column with foreign key constraint
 *
 * The column with foreign key constraint
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#arrrelusingfkeyon
 */
export interface ArrRelUsingFKeyOn {
  column: string;
  table: QualifiedTable | string;
}
/**
 * Manual mapping of table and columns
 *
 * Manual mapping of table and columns
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#arrrelusingmanualmapping
 */
export interface ArrRelUsingManualMapping {
  /**
   * Mapping of columns from current table to remote table
   */
  column_mapping: {
    [key: string]: string;
  };
  /**
   * The table to which the relationship has to be established
   */
  remote_table: QualifiedTable | string;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/computed-field.html#args-syntax
 */
export interface ComputedField {
  /**
   * Comment
   */
  comment?: string;
  /**
   * The computed field definition
   */
  definition: ComputedFieldDefinition;
  /**
   * Name of the new computed field
   */
  name: string;
}
/**
 * The computed field definition
 *
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/computed-field.html#computedfielddefinition
 */
export interface ComputedFieldDefinition {
  /**
   * The SQL function
   */
  function: QualifiedFunction | string;
  /**
   * Name of the argument which accepts the Hasura session object as a JSON/JSONB value. If
   * omitted, the Hasura session object is not passed to the function
   */
  session_argument?: string;
  /**
   * Name of the argument which accepts a table row type. If omitted, the first argument is
   * considered a table argument
   */
  table_argument?: string;
}
/**
 * Configuration for the table/view
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/table-view.html#table-config
 */
export interface TableConfig {
  /**
   * Customise the table name / query root name
   */
  custom_name: string;
  /**
   * Customise the column names
   */
  custom_column_names?: {
    [key: string]: string;
  };
  /**
   * Customise the root fields
   */
  custom_root_fields?: CustomRootFields;
}
/**
 * Customise the root fields
 *
 * Customise the root fields
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/table-view.html#custom-root-fields
 */
export interface CustomRootFields {
  /**
   * Customise the `delete_<table-name>` root field
   */
  delete?: string;
  /**
   * Customise the `delete_<table-name>_by_pk` root field
   */
  delete_by_pk?: string;
  /**
   * Customise the `insert_<table-name>` root field
   */
  insert?: string;
  /**
   * Customise the `insert_<table-name>_one` root field
   */
  insert_one?: string;
  /**
   * Customise the `<table-name>` root field
   */
  select?: string;
  /**
   * Customise the `<table-name>_aggregate` root field
   */
  select_aggregate?: string;
  /**
   * Customise the `<table-name>_by_pk` root field
   */
  select_by_pk?: string;
  /**
   * Customise the `update_<table-name>` root field
   */
  update?: string;
  /**
   * Customise the `update_<table-name>_by_pk` root field
   */
  update_by_pk?: string;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#create-delete-permission-syntax
 */
export interface DeletePermissionEntry {
  /**
   * Comment
   */
  comment?: string;
  /**
   * The permission definition
   */
  permission: DeletePermission;
  /**
   * Role
   */
  role: string;
}
/**
 * The permission definition
 *
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#deletepermission
 */
export interface DeletePermission {
  /**
   * Only the rows where this precondition holds true are updatable
   */
  filter?: {
    [key: string]:
      | number
      | {
          [key: string]: any;
        }
      | string;
  };
}
/**
 * NOTE: The metadata type doesn't QUITE match the 'create' arguments here
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#create-event-trigger
 */
export interface EventTrigger {
  /**
   * The SQL function
   */
  definition: EventTriggerDefinition;
  /**
   * The SQL function
   */
  headers?: Header[];
  /**
   * Name of the event trigger
   */
  name: string;
  /**
   * The SQL function
   */
  retry_conf: RetryConf;
  /**
   * The SQL function
   */
  webhook?: string;
  webhook_from_env?: string;
}
/**
 * The SQL function
 */
export interface EventTriggerDefinition {
  /**
   *
   * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#operationspec
   */
  delete?: OperationSpec;
  enable_manual: boolean;
  /**
   *
   * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#operationspec
   */
  insert?: OperationSpec;
  /**
   *
   * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#operationspec
   */
  update?: OperationSpec;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#operationspec
 */
export interface OperationSpec {
  /**
   *
   * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#eventtriggercolumns
   */
  columns: string[] | Columns;
  /**
   *
   * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#eventtriggercolumns
   */
  payload?: string[] | Columns;
}
export declare enum Columns {
  Empty = '*',
}
/**
 * The SQL function
 *
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/event-triggers.html#retryconf
 */
export interface RetryConf {
  /**
   * Number of seconds to wait between each retry.
   * Default: 10
   */
  interval_sec?: number;
  /**
   * Number of times to retry delivery.
   * Default: 0
   */
  num_retries?: number;
  /**
   * Number of seconds to wait for response before timing out.
   * Default: 60
   */
  timeout_sec?: number;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#args-syntax
 */
export interface InsertPermissionEntry {
  /**
   * Comment
   */
  comment?: string;
  /**
   * The permission definition
   */
  permission: InsertPermission;
  /**
   * Role
   */
  role: string;
}
/**
 * The permission definition
 *
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#insertpermission
 */
export interface InsertPermission {
  /**
   * When set to true the mutation is accessible only if x-hasura-use-backend-only-permissions
   * session variable exists
   * and is set to true and request is made with x-hasura-admin-secret set if any auth is
   * configured
   */
  backend_only?: boolean;
  /**
   * This expression has to hold true for every new row that is inserted
   */
  check?: {
    [key: string]:
      | number
      | {
          [key: string]: any;
        }
      | string;
  };
  /**
   * Can insert into only these columns (or all when '*' is specified)
   */
  columns: string[] | Columns;
  /**
   * Preset values for columns that can be sourced from session variables or static values
   */
  set?: {
    [key: string]: string;
  };
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#args-syntax
 */
export interface ObjectRelationship {
  /**
   * Comment
   */
  comment?: string;
  /**
   * Name of the new relationship
   */
  name: string;
  /**
   * Use one of the available ways to define an object relationship
   */
  using: ObjRelUsing;
}
/**
 * Use one of the available ways to define an object relationship
 *
 * Use one of the available ways to define an object relationship
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#objrelusing
 */
export interface ObjRelUsing {
  /**
   * The column with foreign key constraint
   */
  foreign_key_constraint_on?: string;
  /**
   * Manual mapping of table and columns
   */
  manual_configuration?: ObjRelUsingManualMapping;
}
/**
 * Manual mapping of table and columns
 *
 * Manual mapping of table and columns
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/relationship.html#objrelusingmanualmapping
 */
export interface ObjRelUsingManualMapping {
  /**
   * Mapping of columns from current table to remote table
   */
  column_mapping: {
    [key: string]: string;
  };
  /**
   * The table to which the relationship has to be established
   */
  remote_table: QualifiedTable | string;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/remote-relationships.html#args-syntax
 */
export interface RemoteRelationship {
  /**
   * Definition object
   */
  definition: RemoteRelationshipDef;
  /**
   * Name of the remote relationship
   */
  name: string;
}
/**
 * Definition object
 */
export interface RemoteRelationshipDef {
  /**
   * Column(s) in the table that is used for joining with remote schema field.
   * All join keys in remote_field must appear here.
   */
  hasura_fields: string[];
  /**
   * The schema tree ending at the field in remote schema which needs to be joined with.
   */
  remote_field: {
    [key: string]: RemoteField;
  };
  /**
   * Name of the remote schema to join with
   */
  remote_schema: string;
}
export interface RemoteField {
  arguments: {
    [key: string]: string;
  };
  /**
   * A recursive tree structure that points to the field in the remote schema that needs to be
   * joined with.
   * It is recursive because the remote field maybe nested deeply in the remote schema.
   *
   * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/remote-relationships.html#remotefield
   */
  field?: {
    [key: string]: RemoteField;
  };
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#create-select-permission-syntax
 */
export interface SelectPermissionEntry {
  /**
   * Comment
   */
  comment?: string;
  /**
   * The permission definition
   */
  permission: SelectPermission;
  /**
   * Role
   */
  role: string;
}
/**
 * The permission definition
 *
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#selectpermission
 */
export interface SelectPermission {
  /**
   * Toggle allowing aggregate queries
   */
  allow_aggregations?: boolean;
  /**
   * Only these columns are selectable (or all when '*' is specified)
   */
  columns: string[] | Columns;
  /**
   * Only these computed fields are selectable
   */
  computed_fields?: string[];
  /**
   * Only the rows where this precondition holds true are selectable
   */
  filter?: {
    [key: string]:
      | number
      | {
          [key: string]: any;
        }
      | string;
  };
  /**
   * The maximum number of rows that can be returned
   */
  limit?: number;
}
/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#create-update-permission-syntax
 */
export interface UpdatePermissionEntry {
  /**
   * Comment
   */
  comment?: string;
  /**
   * The permission definition
   */
  permission: UpdatePermission;
  /**
   * Role
   */
  role: string;
}
/**
 * The permission definition
 *
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/permission.html#updatepermission
 */
export interface UpdatePermission {
  /**
   * Postcondition which must be satisfied by rows which have been updated
   */
  check?: {
    [key: string]:
      | number
      | {
          [key: string]: any;
        }
      | string;
  };
  /**
   * Only these columns are selectable (or all when '*' is specified)
   */
  columns: string[] | Columns;
  /**
   * Only the rows where this precondition holds true are updatable
   */
  filter?: {
    [key: string]:
      | number
      | {
          [key: string]: any;
        }
      | string;
  };
  /**
   * Preset values for columns that can be sourced from session variables or static values
   */
  set?: {
    [key: string]: string;
  };
}
export declare class Convert {
  static toPGColumn(json: string): string;
  static pGColumnToJson(value: string): string;
  static toComputedFieldName(json: string): string;
  static computedFieldNameToJson(value: string): string;
  static toRoleName(json: string): string;
  static roleNameToJson(value: string): string;
  static toTriggerName(json: string): string;
  static triggerNameToJson(value: string): string;
  static toRemoteRelationshipName(json: string): string;
  static remoteRelationshipNameToJson(value: string): string;
  static toRemoteSchemaName(json: string): string;
  static remoteSchemaNameToJson(value: string): string;
  static toCollectionName(json: string): string;
  static collectionNameToJson(value: string): string;
  static toGraphQLName(json: string): string;
  static graphQLNameToJson(value: string): string;
  static toGraphQLType(json: string): string;
  static graphQLTypeToJson(value: string): string;
  static toRelationshipName(json: string): string;
  static relationshipNameToJson(value: string): string;
  static toActionName(json: string): string;
  static actionNameToJson(value: string): string;
  static toWebhookURL(json: string): string;
  static webhookURLToJson(value: string): string;
  static toTableName(json: string): QualifiedTable | string;
  static tableNameToJson(value: QualifiedTable | string): string;
  static toQualifiedTable(json: string): QualifiedTable;
  static qualifiedTableToJson(value: QualifiedTable): string;
  static toTableConfig(json: string): TableConfig;
  static tableConfigToJson(value: TableConfig): string;
  static toTableEntry(json: string): TableEntry;
  static tableEntryToJson(value: TableEntry): string;
  static toCustomRootFields(json: string): CustomRootFields;
  static customRootFieldsToJson(value: CustomRootFields): string;
  static toCustomColumnNames(json: string): {
    [key: string]: string;
  };
  static customColumnNamesToJson(value: { [key: string]: string }): string;
  static toFunctionName(json: string): QualifiedFunction | string;
  static functionNameToJson(value: QualifiedFunction | string): string;
  static toQualifiedFunction(json: string): QualifiedFunction;
  static qualifiedFunctionToJson(value: QualifiedFunction): string;
  static toCustomFunction(json: string): CustomFunction;
  static customFunctionToJson(value: CustomFunction): string;
  static toFunctionConfiguration(json: string): FunctionConfiguration;
  static functionConfigurationToJson(value: FunctionConfiguration): string;
  static toObjectRelationship(json: string): ObjectRelationship;
  static objectRelationshipToJson(value: ObjectRelationship): string;
  static toObjRelUsing(json: string): ObjRelUsing;
  static objRelUsingToJson(value: ObjRelUsing): string;
  static toObjRelUsingManualMapping(json: string): ObjRelUsingManualMapping;
  static objRelUsingManualMappingToJson(
    value: ObjRelUsingManualMapping,
  ): string;
  static toArrayRelationship(json: string): ArrayRelationship;
  static arrayRelationshipToJson(value: ArrayRelationship): string;
  static toArrRelUsing(json: string): ArrRelUsing;
  static arrRelUsingToJson(value: ArrRelUsing): string;
  static toArrRelUsingFKeyOn(json: string): ArrRelUsingFKeyOn;
  static arrRelUsingFKeyOnToJson(value: ArrRelUsingFKeyOn): string;
  static toArrRelUsingManualMapping(json: string): ArrRelUsingManualMapping;
  static arrRelUsingManualMappingToJson(
    value: ArrRelUsingManualMapping,
  ): string;
  static toColumnPresetsExpression(json: string): {
    [key: string]: string;
  };
  static columnPresetsExpressionToJson(value: {
    [key: string]: string;
  }): string;
  static toInsertPermissionEntry(json: string): InsertPermissionEntry;
  static insertPermissionEntryToJson(value: InsertPermissionEntry): string;
  static toInsertPermission(json: string): InsertPermission;
  static insertPermissionToJson(value: InsertPermission): string;
  static toSelectPermissionEntry(json: string): SelectPermissionEntry;
  static selectPermissionEntryToJson(value: SelectPermissionEntry): string;
  static toSelectPermission(json: string): SelectPermission;
  static selectPermissionToJson(value: SelectPermission): string;
  static toUpdatePermissionEntry(json: string): UpdatePermissionEntry;
  static updatePermissionEntryToJson(value: UpdatePermissionEntry): string;
  static toUpdatePermission(json: string): UpdatePermission;
  static updatePermissionToJson(value: UpdatePermission): string;
  static toDeletePermissionEntry(json: string): DeletePermissionEntry;
  static deletePermissionEntryToJson(value: DeletePermissionEntry): string;
  static toDeletePermission(json: string): DeletePermission;
  static deletePermissionToJson(value: DeletePermission): string;
  static toComputedField(json: string): ComputedField;
  static computedFieldToJson(value: ComputedField): string;
  static toComputedFieldDefinition(json: string): ComputedFieldDefinition;
  static computedFieldDefinitionToJson(value: ComputedFieldDefinition): string;
  static toEventTrigger(json: string): EventTrigger;
  static eventTriggerToJson(value: EventTrigger): string;
  static toEventTriggerDefinition(json: string): EventTriggerDefinition;
  static eventTriggerDefinitionToJson(value: EventTriggerDefinition): string;
  static toEventTriggerColumns(json: string): string[] | Columns;
  static eventTriggerColumnsToJson(value: string[] | Columns): string;
  static toOperationSpec(json: string): OperationSpec;
  static operationSpecToJson(value: OperationSpec): string;
  static toHeaderFromValue(json: string): HeaderFromValue;
  static headerFromValueToJson(value: HeaderFromValue): string;
  static toHeaderFromEnv(json: string): HeaderFromEnv;
  static headerFromEnvToJson(value: HeaderFromEnv): string;
  static toRetryConf(json: string): RetryConf;
  static retryConfToJson(value: RetryConf): string;
  static toCronTrigger(json: string): CronTrigger;
  static cronTriggerToJson(value: CronTrigger): string;
  static toRetryConfST(json: string): RetryConfST;
  static retryConfSTToJson(value: RetryConfST): string;
  static toRemoteSchema(json: string): RemoteSchema;
  static remoteSchemaToJson(value: RemoteSchema): string;
  static toRemoteSchemaDef(json: string): RemoteSchemaDef;
  static remoteSchemaDefToJson(value: RemoteSchemaDef): string;
  static toRemoteRelationship(json: string): RemoteRelationship;
  static remoteRelationshipToJson(value: RemoteRelationship): string;
  static toRemoteRelationshipDef(json: string): RemoteRelationshipDef;
  static remoteRelationshipDefToJson(value: RemoteRelationshipDef): string;
  static toRemoteField(json: string): {
    [key: string]: RemoteField;
  };
  static remoteFieldToJson(value: { [key: string]: RemoteField }): string;
  static toInputArguments(json: string): {
    [key: string]: string;
  };
  static inputArgumentsToJson(value: { [key: string]: string }): string;
  static toQueryCollectionEntry(json: string): QueryCollectionEntry;
  static queryCollectionEntryToJson(value: QueryCollectionEntry): string;
  static toQueryCollection(json: string): QueryCollection;
  static queryCollectionToJson(value: QueryCollection): string;
  static toAllowList(json: string): AllowList;
  static allowListToJson(value: AllowList): string;
  static toCustomTypes(json: string): CustomTypes;
  static customTypesToJson(value: CustomTypes): string;
  static toInputObjectType(json: string): InputObjectType;
  static inputObjectTypeToJson(value: InputObjectType): string;
  static toInputObjectField(json: string): InputObjectField;
  static inputObjectFieldToJson(value: InputObjectField): string;
  static toObjectType(json: string): ObjectType;
  static objectTypeToJson(value: ObjectType): string;
  static toObjectField(json: string): ObjectField;
  static objectFieldToJson(value: ObjectField): string;
  static toCustomTypeObjectRelationship(
    json: string,
  ): CustomTypeObjectRelationship;
  static customTypeObjectRelationshipToJson(
    value: CustomTypeObjectRelationship,
  ): string;
  static toScalarType(json: string): ScalarType;
  static scalarTypeToJson(value: ScalarType): string;
  static toEnumType(json: string): EnumType;
  static enumTypeToJson(value: EnumType): string;
  static toEnumValue(json: string): EnumValue;
  static enumValueToJson(value: EnumValue): string;
  static toAction(json: string): Action;
  static actionToJson(value: Action): string;
  static toActionDefinition(json: string): ActionDefinition;
  static actionDefinitionToJson(value: ActionDefinition): string;
  static toInputArgument(json: string): InputArgument;
  static inputArgumentToJson(value: InputArgument): string;
  static toHasuraMetadataV2(json: string): HasuraMetadataV2;
  static hasuraMetadataV2ToJson(value: HasuraMetadataV2): string;
}
