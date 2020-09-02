// To parse this data:
//
//   import { Convert, TableName, QualifiedTable, TableConfig, TableEntry, CustomRootFields, FunctionName, QualifiedFunction, CustomFunction, FunctionConfiguration, ObjectRelationship, ObjRelUsing, ObjRelUsingManualMapping, ArrayRelationship, ArrRelUsing, ArrRelUsingFKeyOn, ArrRelUsingManualMapping, InsertPermissionEntry, InsertPermission, SelectPermissionEntry, SelectPermission, UpdatePermissionEntry, UpdatePermission, DeletePermissionEntry, DeletePermission, ComputedField, ComputedFieldDefinition, EventTrigger, EventTriggerDefinition, EventTriggerColumns, OperationSpec, HeaderFromValue, HeaderFromEnv, RetryConf, CronTrigger, RetryConfST, RemoteSchema, RemoteSchemaDef, RemoteRelationship, RemoteRelationshipDef, QueryCollectionEntry, QueryCollection, AllowList, CustomTypes, InputObjectType, InputObjectField, ObjectType, ObjectField, CustomTypeObjectRelationship, ScalarType, EnumType, EnumValue, Action, ActionDefinition, InputArgument, HasuraMetadataV2 } from "./file";
//
//   const pGColumn = Convert.toPGColumn(json);
//   const computedFieldName = Convert.toComputedFieldName(json);
//   const roleName = Convert.toRoleName(json);
//   const triggerName = Convert.toTriggerName(json);
//   const remoteRelationshipName = Convert.toRemoteRelationshipName(json);
//   const remoteSchemaName = Convert.toRemoteSchemaName(json);
//   const collectionName = Convert.toCollectionName(json);
//   const graphQLName = Convert.toGraphQLName(json);
//   const graphQLType = Convert.toGraphQLType(json);
//   const relationshipName = Convert.toRelationshipName(json);
//   const actionName = Convert.toActionName(json);
//   const webhookURL = Convert.toWebhookURL(json);
//   const tableName = Convert.toTableName(json);
//   const qualifiedTable = Convert.toQualifiedTable(json);
//   const tableConfig = Convert.toTableConfig(json);
//   const tableEntry = Convert.toTableEntry(json);
//   const customRootFields = Convert.toCustomRootFields(json);
//   const customColumnNames = Convert.toCustomColumnNames(json);
//   const functionName = Convert.toFunctionName(json);
//   const qualifiedFunction = Convert.toQualifiedFunction(json);
//   const customFunction = Convert.toCustomFunction(json);
//   const functionConfiguration = Convert.toFunctionConfiguration(json);
//   const objectRelationship = Convert.toObjectRelationship(json);
//   const objRelUsing = Convert.toObjRelUsing(json);
//   const objRelUsingManualMapping = Convert.toObjRelUsingManualMapping(json);
//   const arrayRelationship = Convert.toArrayRelationship(json);
//   const arrRelUsing = Convert.toArrRelUsing(json);
//   const arrRelUsingFKeyOn = Convert.toArrRelUsingFKeyOn(json);
//   const arrRelUsingManualMapping = Convert.toArrRelUsingManualMapping(json);
//   const columnPresetsExpression = Convert.toColumnPresetsExpression(json);
//   const insertPermissionEntry = Convert.toInsertPermissionEntry(json);
//   const insertPermission = Convert.toInsertPermission(json);
//   const selectPermissionEntry = Convert.toSelectPermissionEntry(json);
//   const selectPermission = Convert.toSelectPermission(json);
//   const updatePermissionEntry = Convert.toUpdatePermissionEntry(json);
//   const updatePermission = Convert.toUpdatePermission(json);
//   const deletePermissionEntry = Convert.toDeletePermissionEntry(json);
//   const deletePermission = Convert.toDeletePermission(json);
//   const computedField = Convert.toComputedField(json);
//   const computedFieldDefinition = Convert.toComputedFieldDefinition(json);
//   const eventTrigger = Convert.toEventTrigger(json);
//   const eventTriggerDefinition = Convert.toEventTriggerDefinition(json);
//   const eventTriggerColumns = Convert.toEventTriggerColumns(json);
//   const operationSpec = Convert.toOperationSpec(json);
//   const headerFromValue = Convert.toHeaderFromValue(json);
//   const headerFromEnv = Convert.toHeaderFromEnv(json);
//   const retryConf = Convert.toRetryConf(json);
//   const cronTrigger = Convert.toCronTrigger(json);
//   const retryConfST = Convert.toRetryConfST(json);
//   const remoteSchema = Convert.toRemoteSchema(json);
//   const remoteSchemaDef = Convert.toRemoteSchemaDef(json);
//   const remoteRelationship = Convert.toRemoteRelationship(json);
//   const remoteRelationshipDef = Convert.toRemoteRelationshipDef(json);
//   const remoteField = Convert.toRemoteField(json);
//   const inputArguments = Convert.toInputArguments(json);
//   const queryCollectionEntry = Convert.toQueryCollectionEntry(json);
//   const queryCollection = Convert.toQueryCollection(json);
//   const allowList = Convert.toAllowList(json);
//   const customTypes = Convert.toCustomTypes(json);
//   const inputObjectType = Convert.toInputObjectType(json);
//   const inputObjectField = Convert.toInputObjectField(json);
//   const objectType = Convert.toObjectType(json);
//   const objectField = Convert.toObjectField(json);
//   const customTypeObjectRelationship = Convert.toCustomTypeObjectRelationship(json);
//   const scalarType = Convert.toScalarType(json);
//   const enumType = Convert.toEnumType(json);
//   const enumValue = Convert.toEnumValue(json);
//   const action = Convert.toAction(json);
//   const actionDefinition = Convert.toActionDefinition(json);
//   const inputArgument = Convert.toInputArgument(json);
//   const hasuraMetadataV2 = Convert.toHasuraMetadataV2(json);
//
// These functions will throw an error if the JSON doesn't
// match the expected interface, even if the JSON is valid.

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromvalue
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromenv
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#objectfield
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/manage-metadata.html#replace-metadata
 */
export interface HasuraMetadataV2 {
    actions?:           Action[];
    allowlist?:         AllowList[];
    cron_triggers?:     CronTrigger[];
    custom_types?:      CustomTypes;
    functions?:         CustomFunction[];
    query_collections?: QueryCollectionEntry[];
    remote_schemas?:    RemoteSchema[];
    tables:             TableEntry[];
    version:            number;
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/actions.html#args-syntax
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/actions.html#actiondefinition
 */
export interface ActionDefinition {
    arguments?:              InputArgument[];
    forward_client_headers?: boolean;
    /**
     * A String value which supports templating environment variables enclosed in {{ and }}.
     * Template example: https://{{ACTION_API_DOMAIN}}/create-user
     */
    handler:      string;
    headers?:     Header[];
    kind?:        string;
    output_type?: string;
    type?:        ActionDefinitionType;
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/actions.html#inputargument
 */
export interface InputArgument {
    name: string;
    type: string;
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromvalue
 *
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromenv
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

export enum ActionDefinitionType {
    Mutation = "mutation",
    Query = "query",
}

/**
 * Permissions of the action
 */
export interface Permissions {
    role: string;
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/query-collections.html#add-collection-to-allowlist-syntax
 */
export interface AllowList {
    /**
     * Name of a query collection to be added to the allow-list
     */
    collection: string;
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/scheduled-triggers.html#create-cron-trigger
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
    payload?: { [key: string]: any };
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/scheduled-triggers.html#retryconfst
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
    enums?:         EnumType[];
    input_objects?: InputObjectType[];
    objects?:       ObjectType[];
    scalars?:       ScalarType[];
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#enumtype
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#enumvalue
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#inputobjecttype
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#inputobjectfield
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#objecttype
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#objectrelationship
 */
export interface CustomTypeObjectRelationship {
    /**
     * Mapping of fields of object type to columns of remote table
     */
    field_mapping: { [key: string]: string };
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
    name:   string;
    schema: string;
}

/**
 * Type of the relationship
 */
export enum CustomTypeObjectRelationshipType {
    Array = "array",
    Object = "object",
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#scalartype
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-functions.html#args-syntax
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-functions.html#function-configuration
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
    name:   string;
    schema: string;
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/query-collections.html#args-syntax
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#collectionquery
 */
export interface QueryCollection {
    name:  string;
    query: string;
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/remote-schemas.html#add-remote-schema
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#remoteschemadef
 */
export interface RemoteSchemaDef {
    forward_client_headers?: boolean;
    headers?:                Header[];
    timeout_seconds?:        number;
    url?:                    string;
    url_from_env?:           string;
}

/**
 * Representation of a table in metadata, 'tables.yaml' and 'metadata.json'
 */
export interface TableEntry {
    array_relationships?: ArrayRelationship[];
    computed_fields?:     ComputedField[];
    /**
     * Configuration for the table/view
     *
     * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/table-view.html#table-config
     */
    configuration?:        TableConfig;
    delete_permissions?:   DeletePermissionEntry[];
    event_triggers?:       EventTrigger[];
    insert_permissions?:   InsertPermissionEntry[];
    is_enum?:              boolean;
    object_relationships?: ObjectRelationship[];
    remote_relationships?: RemoteRelationship[];
    select_permissions?:   SelectPermissionEntry[];
    table:                 QualifiedTable;
    update_permissions?:   UpdatePermissionEntry[];
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#create-array-relationship-syntax
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#arrrelusing
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#arrrelusingfkeyon
 */
export interface ArrRelUsingFKeyOn {
    column: string;
    table:  QualifiedTable | string;
}

/**
 * Manual mapping of table and columns
 *
 * Manual mapping of table and columns
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#arrrelusingmanualmapping
 */
export interface ArrRelUsingManualMapping {
    /**
     * Mapping of columns from current table to remote table
     */
    column_mapping: { [key: string]: string };
    /**
     * The table to which the relationship has to be established
     */
    remote_table: QualifiedTable | string;
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/computed-field.html#args-syntax
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/computed-field.html#computedfielddefinition
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/table-view.html#table-config
 */
export interface TableConfig {
    /**
     * Customise the column names
     */
    custom_column_names?: { [key: string]: string };
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/table-view.html#custom-root-fields
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#create-delete-permission-syntax
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#deletepermission
 */
export interface DeletePermission {
    /**
     * Only the rows where this precondition holds true are updatable
     */
    filter?: { [key: string]: number | { [key: string]: any } | string };
}

/**
 * NOTE: The metadata type doesn't QUITE match the 'create' arguments here
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#create-event-trigger
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
    webhook?:          string;
    webhook_from_env?: string;
}

/**
 * The SQL function
 */
export interface EventTriggerDefinition {
    /**
     *
     * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec
     */
    delete?:       OperationSpec;
    enable_manual: boolean;
    /**
     *
     * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec
     */
    insert?: OperationSpec;
    /**
     *
     * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec
     */
    update?: OperationSpec;
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec
 */
export interface OperationSpec {
    /**
     *
     * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#eventtriggercolumns
     */
    columns: string[] | Columns;
    /**
     *
     * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#eventtriggercolumns
     */
    payload?: string[] | Columns;
}

export enum Columns {
    Empty = "*",
}

/**
 * The SQL function
 *
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#retryconf
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#args-syntax
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#insertpermission
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
    check?: { [key: string]: number | { [key: string]: any } | string };
    /**
     * Can insert into only these columns (or all when '*' is specified)
     */
    columns: string[] | Columns;
    /**
     * Preset values for columns that can be sourced from session variables or static values
     */
    set?: { [key: string]: string };
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#args-syntax
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#objrelusing
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#objrelusingmanualmapping
 */
export interface ObjRelUsingManualMapping {
    /**
     * Mapping of columns from current table to remote table
     */
    column_mapping: { [key: string]: string };
    /**
     * The table to which the relationship has to be established
     */
    remote_table: QualifiedTable | string;
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/remote-relationships.html#args-syntax
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
    remote_field: { [key: string]: RemoteField };
    /**
     * Name of the remote schema to join with
     */
    remote_schema: string;
}

export interface RemoteField {
    arguments: { [key: string]: string };
    /**
     * A recursive tree structure that points to the field in the remote schema that needs to be
     * joined with.
     * It is recursive because the remote field maybe nested deeply in the remote schema.
     *
     * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/remote-relationships.html#remotefield
     */
    field?: { [key: string]: RemoteField };
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#create-select-permission-syntax
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#selectpermission
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
    filter?: { [key: string]: number | { [key: string]: any } | string };
    /**
     * The maximum number of rows that can be returned
     */
    limit?: number;
}

/**
 *
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#create-update-permission-syntax
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
 * https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#updatepermission
 */
export interface UpdatePermission {
    /**
     * Postcondition which must be satisfied by rows which have been updated
     */
    check?: { [key: string]: number | { [key: string]: any } | string };
    /**
     * Only these columns are selectable (or all when '*' is specified)
     */
    columns: string[] | Columns;
    /**
     * Only the rows where this precondition holds true are updatable
     */
    filter?: { [key: string]: number | { [key: string]: any } | string };
    /**
     * Preset values for columns that can be sourced from session variables or static values
     */
    set?: { [key: string]: string };
}

// Converts JSON strings to/from your types
// and asserts the results of JSON.parse at runtime
export class Convert {
    public static toPGColumn(json: string): string {
        return cast(JSON.parse(json), "");
    }

    public static pGColumnToJson(value: string): string {
        return JSON.stringify(uncast(value, ""), null, 2);
    }

    public static toComputedFieldName(json: string): string {
        return cast(JSON.parse(json), "");
    }

    public static computedFieldNameToJson(value: string): string {
        return JSON.stringify(uncast(value, ""), null, 2);
    }

    public static toRoleName(json: string): string {
        return cast(JSON.parse(json), "");
    }

    public static roleNameToJson(value: string): string {
        return JSON.stringify(uncast(value, ""), null, 2);
    }

    public static toTriggerName(json: string): string {
        return cast(JSON.parse(json), "");
    }

    public static triggerNameToJson(value: string): string {
        return JSON.stringify(uncast(value, ""), null, 2);
    }

    public static toRemoteRelationshipName(json: string): string {
        return cast(JSON.parse(json), "");
    }

    public static remoteRelationshipNameToJson(value: string): string {
        return JSON.stringify(uncast(value, ""), null, 2);
    }

    public static toRemoteSchemaName(json: string): string {
        return cast(JSON.parse(json), "");
    }

    public static remoteSchemaNameToJson(value: string): string {
        return JSON.stringify(uncast(value, ""), null, 2);
    }

    public static toCollectionName(json: string): string {
        return cast(JSON.parse(json), "");
    }

    public static collectionNameToJson(value: string): string {
        return JSON.stringify(uncast(value, ""), null, 2);
    }

    public static toGraphQLName(json: string): string {
        return cast(JSON.parse(json), "");
    }

    public static graphQLNameToJson(value: string): string {
        return JSON.stringify(uncast(value, ""), null, 2);
    }

    public static toGraphQLType(json: string): string {
        return cast(JSON.parse(json), "");
    }

    public static graphQLTypeToJson(value: string): string {
        return JSON.stringify(uncast(value, ""), null, 2);
    }

    public static toRelationshipName(json: string): string {
        return cast(JSON.parse(json), "");
    }

    public static relationshipNameToJson(value: string): string {
        return JSON.stringify(uncast(value, ""), null, 2);
    }

    public static toActionName(json: string): string {
        return cast(JSON.parse(json), "");
    }

    public static actionNameToJson(value: string): string {
        return JSON.stringify(uncast(value, ""), null, 2);
    }

    public static toWebhookURL(json: string): string {
        return cast(JSON.parse(json), "");
    }

    public static webhookURLToJson(value: string): string {
        return JSON.stringify(uncast(value, ""), null, 2);
    }

    public static toTableName(json: string): QualifiedTable | string {
        return cast(JSON.parse(json), u(r("QualifiedTable"), ""));
    }

    public static tableNameToJson(value: QualifiedTable | string): string {
        return JSON.stringify(uncast(value, u(r("QualifiedTable"), "")), null, 2);
    }

    public static toQualifiedTable(json: string): QualifiedTable {
        return cast(JSON.parse(json), r("QualifiedTable"));
    }

    public static qualifiedTableToJson(value: QualifiedTable): string {
        return JSON.stringify(uncast(value, r("QualifiedTable")), null, 2);
    }

    public static toTableConfig(json: string): TableConfig {
        return cast(JSON.parse(json), r("TableConfig"));
    }

    public static tableConfigToJson(value: TableConfig): string {
        return JSON.stringify(uncast(value, r("TableConfig")), null, 2);
    }

    public static toTableEntry(json: string): TableEntry {
        return cast(JSON.parse(json), r("TableEntry"));
    }

    public static tableEntryToJson(value: TableEntry): string {
        return JSON.stringify(uncast(value, r("TableEntry")), null, 2);
    }

    public static toCustomRootFields(json: string): CustomRootFields {
        return cast(JSON.parse(json), r("CustomRootFields"));
    }

    public static customRootFieldsToJson(value: CustomRootFields): string {
        return JSON.stringify(uncast(value, r("CustomRootFields")), null, 2);
    }

    public static toCustomColumnNames(json: string): { [key: string]: string } {
        return cast(JSON.parse(json), m(""));
    }

    public static customColumnNamesToJson(value: { [key: string]: string }): string {
        return JSON.stringify(uncast(value, m("")), null, 2);
    }

    public static toFunctionName(json: string): QualifiedFunction | string {
        return cast(JSON.parse(json), u(r("QualifiedFunction"), ""));
    }

    public static functionNameToJson(value: QualifiedFunction | string): string {
        return JSON.stringify(uncast(value, u(r("QualifiedFunction"), "")), null, 2);
    }

    public static toQualifiedFunction(json: string): QualifiedFunction {
        return cast(JSON.parse(json), r("QualifiedFunction"));
    }

    public static qualifiedFunctionToJson(value: QualifiedFunction): string {
        return JSON.stringify(uncast(value, r("QualifiedFunction")), null, 2);
    }

    public static toCustomFunction(json: string): CustomFunction {
        return cast(JSON.parse(json), r("CustomFunction"));
    }

    public static customFunctionToJson(value: CustomFunction): string {
        return JSON.stringify(uncast(value, r("CustomFunction")), null, 2);
    }

    public static toFunctionConfiguration(json: string): FunctionConfiguration {
        return cast(JSON.parse(json), r("FunctionConfiguration"));
    }

    public static functionConfigurationToJson(value: FunctionConfiguration): string {
        return JSON.stringify(uncast(value, r("FunctionConfiguration")), null, 2);
    }

    public static toObjectRelationship(json: string): ObjectRelationship {
        return cast(JSON.parse(json), r("ObjectRelationship"));
    }

    public static objectRelationshipToJson(value: ObjectRelationship): string {
        return JSON.stringify(uncast(value, r("ObjectRelationship")), null, 2);
    }

    public static toObjRelUsing(json: string): ObjRelUsing {
        return cast(JSON.parse(json), r("ObjRelUsing"));
    }

    public static objRelUsingToJson(value: ObjRelUsing): string {
        return JSON.stringify(uncast(value, r("ObjRelUsing")), null, 2);
    }

    public static toObjRelUsingManualMapping(json: string): ObjRelUsingManualMapping {
        return cast(JSON.parse(json), r("ObjRelUsingManualMapping"));
    }

    public static objRelUsingManualMappingToJson(value: ObjRelUsingManualMapping): string {
        return JSON.stringify(uncast(value, r("ObjRelUsingManualMapping")), null, 2);
    }

    public static toArrayRelationship(json: string): ArrayRelationship {
        return cast(JSON.parse(json), r("ArrayRelationship"));
    }

    public static arrayRelationshipToJson(value: ArrayRelationship): string {
        return JSON.stringify(uncast(value, r("ArrayRelationship")), null, 2);
    }

    public static toArrRelUsing(json: string): ArrRelUsing {
        return cast(JSON.parse(json), r("ArrRelUsing"));
    }

    public static arrRelUsingToJson(value: ArrRelUsing): string {
        return JSON.stringify(uncast(value, r("ArrRelUsing")), null, 2);
    }

    public static toArrRelUsingFKeyOn(json: string): ArrRelUsingFKeyOn {
        return cast(JSON.parse(json), r("ArrRelUsingFKeyOn"));
    }

    public static arrRelUsingFKeyOnToJson(value: ArrRelUsingFKeyOn): string {
        return JSON.stringify(uncast(value, r("ArrRelUsingFKeyOn")), null, 2);
    }

    public static toArrRelUsingManualMapping(json: string): ArrRelUsingManualMapping {
        return cast(JSON.parse(json), r("ArrRelUsingManualMapping"));
    }

    public static arrRelUsingManualMappingToJson(value: ArrRelUsingManualMapping): string {
        return JSON.stringify(uncast(value, r("ArrRelUsingManualMapping")), null, 2);
    }

    public static toColumnPresetsExpression(json: string): { [key: string]: string } {
        return cast(JSON.parse(json), m(""));
    }

    public static columnPresetsExpressionToJson(value: { [key: string]: string }): string {
        return JSON.stringify(uncast(value, m("")), null, 2);
    }

    public static toInsertPermissionEntry(json: string): InsertPermissionEntry {
        return cast(JSON.parse(json), r("InsertPermissionEntry"));
    }

    public static insertPermissionEntryToJson(value: InsertPermissionEntry): string {
        return JSON.stringify(uncast(value, r("InsertPermissionEntry")), null, 2);
    }

    public static toInsertPermission(json: string): InsertPermission {
        return cast(JSON.parse(json), r("InsertPermission"));
    }

    public static insertPermissionToJson(value: InsertPermission): string {
        return JSON.stringify(uncast(value, r("InsertPermission")), null, 2);
    }

    public static toSelectPermissionEntry(json: string): SelectPermissionEntry {
        return cast(JSON.parse(json), r("SelectPermissionEntry"));
    }

    public static selectPermissionEntryToJson(value: SelectPermissionEntry): string {
        return JSON.stringify(uncast(value, r("SelectPermissionEntry")), null, 2);
    }

    public static toSelectPermission(json: string): SelectPermission {
        return cast(JSON.parse(json), r("SelectPermission"));
    }

    public static selectPermissionToJson(value: SelectPermission): string {
        return JSON.stringify(uncast(value, r("SelectPermission")), null, 2);
    }

    public static toUpdatePermissionEntry(json: string): UpdatePermissionEntry {
        return cast(JSON.parse(json), r("UpdatePermissionEntry"));
    }

    public static updatePermissionEntryToJson(value: UpdatePermissionEntry): string {
        return JSON.stringify(uncast(value, r("UpdatePermissionEntry")), null, 2);
    }

    public static toUpdatePermission(json: string): UpdatePermission {
        return cast(JSON.parse(json), r("UpdatePermission"));
    }

    public static updatePermissionToJson(value: UpdatePermission): string {
        return JSON.stringify(uncast(value, r("UpdatePermission")), null, 2);
    }

    public static toDeletePermissionEntry(json: string): DeletePermissionEntry {
        return cast(JSON.parse(json), r("DeletePermissionEntry"));
    }

    public static deletePermissionEntryToJson(value: DeletePermissionEntry): string {
        return JSON.stringify(uncast(value, r("DeletePermissionEntry")), null, 2);
    }

    public static toDeletePermission(json: string): DeletePermission {
        return cast(JSON.parse(json), r("DeletePermission"));
    }

    public static deletePermissionToJson(value: DeletePermission): string {
        return JSON.stringify(uncast(value, r("DeletePermission")), null, 2);
    }

    public static toComputedField(json: string): ComputedField {
        return cast(JSON.parse(json), r("ComputedField"));
    }

    public static computedFieldToJson(value: ComputedField): string {
        return JSON.stringify(uncast(value, r("ComputedField")), null, 2);
    }

    public static toComputedFieldDefinition(json: string): ComputedFieldDefinition {
        return cast(JSON.parse(json), r("ComputedFieldDefinition"));
    }

    public static computedFieldDefinitionToJson(value: ComputedFieldDefinition): string {
        return JSON.stringify(uncast(value, r("ComputedFieldDefinition")), null, 2);
    }

    public static toEventTrigger(json: string): EventTrigger {
        return cast(JSON.parse(json), r("EventTrigger"));
    }

    public static eventTriggerToJson(value: EventTrigger): string {
        return JSON.stringify(uncast(value, r("EventTrigger")), null, 2);
    }

    public static toEventTriggerDefinition(json: string): EventTriggerDefinition {
        return cast(JSON.parse(json), r("EventTriggerDefinition"));
    }

    public static eventTriggerDefinitionToJson(value: EventTriggerDefinition): string {
        return JSON.stringify(uncast(value, r("EventTriggerDefinition")), null, 2);
    }

    public static toEventTriggerColumns(json: string): string[] | Columns {
        return cast(JSON.parse(json), u(a(""), r("Columns")));
    }

    public static eventTriggerColumnsToJson(value: string[] | Columns): string {
        return JSON.stringify(uncast(value, u(a(""), r("Columns"))), null, 2);
    }

    public static toOperationSpec(json: string): OperationSpec {
        return cast(JSON.parse(json), r("OperationSpec"));
    }

    public static operationSpecToJson(value: OperationSpec): string {
        return JSON.stringify(uncast(value, r("OperationSpec")), null, 2);
    }

    public static toHeaderFromValue(json: string): HeaderFromValue {
        return cast(JSON.parse(json), r("HeaderFromValue"));
    }

    public static headerFromValueToJson(value: HeaderFromValue): string {
        return JSON.stringify(uncast(value, r("HeaderFromValue")), null, 2);
    }

    public static toHeaderFromEnv(json: string): HeaderFromEnv {
        return cast(JSON.parse(json), r("HeaderFromEnv"));
    }

    public static headerFromEnvToJson(value: HeaderFromEnv): string {
        return JSON.stringify(uncast(value, r("HeaderFromEnv")), null, 2);
    }

    public static toRetryConf(json: string): RetryConf {
        return cast(JSON.parse(json), r("RetryConf"));
    }

    public static retryConfToJson(value: RetryConf): string {
        return JSON.stringify(uncast(value, r("RetryConf")), null, 2);
    }

    public static toCronTrigger(json: string): CronTrigger {
        return cast(JSON.parse(json), r("CronTrigger"));
    }

    public static cronTriggerToJson(value: CronTrigger): string {
        return JSON.stringify(uncast(value, r("CronTrigger")), null, 2);
    }

    public static toRetryConfST(json: string): RetryConfST {
        return cast(JSON.parse(json), r("RetryConfST"));
    }

    public static retryConfSTToJson(value: RetryConfST): string {
        return JSON.stringify(uncast(value, r("RetryConfST")), null, 2);
    }

    public static toRemoteSchema(json: string): RemoteSchema {
        return cast(JSON.parse(json), r("RemoteSchema"));
    }

    public static remoteSchemaToJson(value: RemoteSchema): string {
        return JSON.stringify(uncast(value, r("RemoteSchema")), null, 2);
    }

    public static toRemoteSchemaDef(json: string): RemoteSchemaDef {
        return cast(JSON.parse(json), r("RemoteSchemaDef"));
    }

    public static remoteSchemaDefToJson(value: RemoteSchemaDef): string {
        return JSON.stringify(uncast(value, r("RemoteSchemaDef")), null, 2);
    }

    public static toRemoteRelationship(json: string): RemoteRelationship {
        return cast(JSON.parse(json), r("RemoteRelationship"));
    }

    public static remoteRelationshipToJson(value: RemoteRelationship): string {
        return JSON.stringify(uncast(value, r("RemoteRelationship")), null, 2);
    }

    public static toRemoteRelationshipDef(json: string): RemoteRelationshipDef {
        return cast(JSON.parse(json), r("RemoteRelationshipDef"));
    }

    public static remoteRelationshipDefToJson(value: RemoteRelationshipDef): string {
        return JSON.stringify(uncast(value, r("RemoteRelationshipDef")), null, 2);
    }

    public static toRemoteField(json: string): { [key: string]: RemoteField } {
        return cast(JSON.parse(json), m(r("RemoteField")));
    }

    public static remoteFieldToJson(value: { [key: string]: RemoteField }): string {
        return JSON.stringify(uncast(value, m(r("RemoteField"))), null, 2);
    }

    public static toInputArguments(json: string): { [key: string]: string } {
        return cast(JSON.parse(json), m(""));
    }

    public static inputArgumentsToJson(value: { [key: string]: string }): string {
        return JSON.stringify(uncast(value, m("")), null, 2);
    }

    public static toQueryCollectionEntry(json: string): QueryCollectionEntry {
        return cast(JSON.parse(json), r("QueryCollectionEntry"));
    }

    public static queryCollectionEntryToJson(value: QueryCollectionEntry): string {
        return JSON.stringify(uncast(value, r("QueryCollectionEntry")), null, 2);
    }

    public static toQueryCollection(json: string): QueryCollection {
        return cast(JSON.parse(json), r("QueryCollection"));
    }

    public static queryCollectionToJson(value: QueryCollection): string {
        return JSON.stringify(uncast(value, r("QueryCollection")), null, 2);
    }

    public static toAllowList(json: string): AllowList {
        return cast(JSON.parse(json), r("AllowList"));
    }

    public static allowListToJson(value: AllowList): string {
        return JSON.stringify(uncast(value, r("AllowList")), null, 2);
    }

    public static toCustomTypes(json: string): CustomTypes {
        return cast(JSON.parse(json), r("CustomTypes"));
    }

    public static customTypesToJson(value: CustomTypes): string {
        return JSON.stringify(uncast(value, r("CustomTypes")), null, 2);
    }

    public static toInputObjectType(json: string): InputObjectType {
        return cast(JSON.parse(json), r("InputObjectType"));
    }

    public static inputObjectTypeToJson(value: InputObjectType): string {
        return JSON.stringify(uncast(value, r("InputObjectType")), null, 2);
    }

    public static toInputObjectField(json: string): InputObjectField {
        return cast(JSON.parse(json), r("InputObjectField"));
    }

    public static inputObjectFieldToJson(value: InputObjectField): string {
        return JSON.stringify(uncast(value, r("InputObjectField")), null, 2);
    }

    public static toObjectType(json: string): ObjectType {
        return cast(JSON.parse(json), r("ObjectType"));
    }

    public static objectTypeToJson(value: ObjectType): string {
        return JSON.stringify(uncast(value, r("ObjectType")), null, 2);
    }

    public static toObjectField(json: string): ObjectField {
        return cast(JSON.parse(json), r("ObjectField"));
    }

    public static objectFieldToJson(value: ObjectField): string {
        return JSON.stringify(uncast(value, r("ObjectField")), null, 2);
    }

    public static toCustomTypeObjectRelationship(json: string): CustomTypeObjectRelationship {
        return cast(JSON.parse(json), r("CustomTypeObjectRelationship"));
    }

    public static customTypeObjectRelationshipToJson(value: CustomTypeObjectRelationship): string {
        return JSON.stringify(uncast(value, r("CustomTypeObjectRelationship")), null, 2);
    }

    public static toScalarType(json: string): ScalarType {
        return cast(JSON.parse(json), r("ScalarType"));
    }

    public static scalarTypeToJson(value: ScalarType): string {
        return JSON.stringify(uncast(value, r("ScalarType")), null, 2);
    }

    public static toEnumType(json: string): EnumType {
        return cast(JSON.parse(json), r("EnumType"));
    }

    public static enumTypeToJson(value: EnumType): string {
        return JSON.stringify(uncast(value, r("EnumType")), null, 2);
    }

    public static toEnumValue(json: string): EnumValue {
        return cast(JSON.parse(json), r("EnumValue"));
    }

    public static enumValueToJson(value: EnumValue): string {
        return JSON.stringify(uncast(value, r("EnumValue")), null, 2);
    }

    public static toAction(json: string): Action {
        return cast(JSON.parse(json), r("Action"));
    }

    public static actionToJson(value: Action): string {
        return JSON.stringify(uncast(value, r("Action")), null, 2);
    }

    public static toActionDefinition(json: string): ActionDefinition {
        return cast(JSON.parse(json), r("ActionDefinition"));
    }

    public static actionDefinitionToJson(value: ActionDefinition): string {
        return JSON.stringify(uncast(value, r("ActionDefinition")), null, 2);
    }

    public static toInputArgument(json: string): InputArgument {
        return cast(JSON.parse(json), r("InputArgument"));
    }

    public static inputArgumentToJson(value: InputArgument): string {
        return JSON.stringify(uncast(value, r("InputArgument")), null, 2);
    }

    public static toHasuraMetadataV2(json: string): HasuraMetadataV2 {
        return cast(JSON.parse(json), r("HasuraMetadataV2"));
    }

    public static hasuraMetadataV2ToJson(value: HasuraMetadataV2): string {
        return JSON.stringify(uncast(value, r("HasuraMetadataV2")), null, 2);
    }
}

function invalidValue(typ: any, val: any): never {
    throw Error(`Invalid value ${JSON.stringify(val)} for type ${JSON.stringify(typ)}`);
}

function jsonToJSProps(typ: any): any {
    if (typ.jsonToJS === undefined) {
        const map: any = {};
        typ.props.forEach((p: any) => map[p.json] = { key: p.js, typ: p.typ });
        typ.jsonToJS = map;
    }
    return typ.jsonToJS;
}

function jsToJSONProps(typ: any): any {
    if (typ.jsToJSON === undefined) {
        const map: any = {};
        typ.props.forEach((p: any) => map[p.js] = { key: p.json, typ: p.typ });
        typ.jsToJSON = map;
    }
    return typ.jsToJSON;
}

function transform(val: any, typ: any, getProps: any): any {
    function transformPrimitive(typ: string, val: any): any {
        if (typeof typ === typeof val) return val;
        return invalidValue(typ, val);
    }

    function transformUnion(typs: any[], val: any): any {
        // val must validate against one typ in typs
        const l = typs.length;
        for (let i = 0; i < l; i++) {
            const typ = typs[i];
            try {
                return transform(val, typ, getProps);
            } catch (_) {}
        }
        return invalidValue(typs, val);
    }

    function transformEnum(cases: string[], val: any): any {
        if (cases.indexOf(val) !== -1) return val;
        return invalidValue(cases, val);
    }

    function transformArray(typ: any, val: any): any {
        // val must be an array with no invalid elements
        if (!Array.isArray(val)) return invalidValue("array", val);
        return val.map(el => transform(el, typ, getProps));
    }

    function transformDate(val: any): any {
        if (val === null) {
            return null;
        }
        const d = new Date(val);
        if (isNaN(d.valueOf())) {
            return invalidValue("Date", val);
        }
        return d;
    }

    function transformObject(props: { [k: string]: any }, additional: any, val: any): any {
        if (val === null || typeof val !== "object" || Array.isArray(val)) {
            return invalidValue("object", val);
        }
        const result: any = {};
        Object.getOwnPropertyNames(props).forEach(key => {
            const prop = props[key];
            const v = Object.prototype.hasOwnProperty.call(val, key) ? val[key] : undefined;
            result[prop.key] = transform(v, prop.typ, getProps);
        });
        Object.getOwnPropertyNames(val).forEach(key => {
            if (!Object.prototype.hasOwnProperty.call(props, key)) {
                result[key] = transform(val[key], additional, getProps);
            }
        });
        return result;
    }

    if (typ === "any") return val;
    if (typ === null) {
        if (val === null) return val;
        return invalidValue(typ, val);
    }
    if (typ === false) return invalidValue(typ, val);
    while (typeof typ === "object" && typ.ref !== undefined) {
        typ = typeMap[typ.ref];
    }
    if (Array.isArray(typ)) return transformEnum(typ, val);
    if (typeof typ === "object") {
        return typ.hasOwnProperty("unionMembers") ? transformUnion(typ.unionMembers, val)
            : typ.hasOwnProperty("arrayItems")    ? transformArray(typ.arrayItems, val)
            : typ.hasOwnProperty("props")         ? transformObject(getProps(typ), typ.additional, val)
            : invalidValue(typ, val);
    }
    // Numbers can be parsed by Date but shouldn't be.
    if (typ === Date && typeof val !== "number") return transformDate(val);
    return transformPrimitive(typ, val);
}

function cast<T>(val: any, typ: any): T {
    return transform(val, typ, jsonToJSProps);
}

function uncast<T>(val: T, typ: any): any {
    return transform(val, typ, jsToJSONProps);
}

function a(typ: any) {
    return { arrayItems: typ };
}

function u(...typs: any[]) {
    return { unionMembers: typs };
}

function o(props: any[], additional: any) {
    return { props, additional };
}

function m(additional: any) {
    return { props: [], additional };
}

function r(name: string) {
    return { ref: name };
}

const typeMap: any = {
    "HeaderFromValue": o([
        { json: "name", js: "name", typ: "" },
        { json: "value", js: "value", typ: "" },
    ], "any"),
    "HeaderFromEnv": o([
        { json: "name", js: "name", typ: "" },
        { json: "value_from_env", js: "value_from_env", typ: "" },
    ], "any"),
    "ObjectField": o([
        { json: "description", js: "description", typ: u(undefined, "") },
        { json: "name", js: "name", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], "any"),
    "HasuraMetadataV2": o([
        { json: "actions", js: "actions", typ: u(undefined, a(r("Action"))) },
        { json: "allowlist", js: "allowlist", typ: u(undefined, a(r("AllowList"))) },
        { json: "cron_triggers", js: "cron_triggers", typ: u(undefined, a(r("CronTrigger"))) },
        { json: "custom_types", js: "custom_types", typ: u(undefined, r("CustomTypes")) },
        { json: "functions", js: "functions", typ: u(undefined, a(r("CustomFunction"))) },
        { json: "query_collections", js: "query_collections", typ: u(undefined, a(r("QueryCollectionEntry"))) },
        { json: "remote_schemas", js: "remote_schemas", typ: u(undefined, a(r("RemoteSchema"))) },
        { json: "tables", js: "tables", typ: a(r("TableEntry")) },
        { json: "version", js: "version", typ: 3.14 },
    ], "any"),
    "Action": o([
        { json: "comment", js: "comment", typ: u(undefined, "") },
        { json: "definition", js: "definition", typ: r("ActionDefinition") },
        { json: "name", js: "name", typ: "" },
        { json: "permissions", js: "permissions", typ: u(undefined, r("Permissions")) },
    ], "any"),
    "ActionDefinition": o([
        { json: "arguments", js: "arguments", typ: u(undefined, a(r("InputArgument"))) },
        { json: "forward_client_headers", js: "forward_client_headers", typ: u(undefined, true) },
        { json: "handler", js: "handler", typ: "" },
        { json: "headers", js: "headers", typ: u(undefined, a(r("Header"))) },
        { json: "kind", js: "kind", typ: u(undefined, "") },
        { json: "output_type", js: "output_type", typ: u(undefined, "") },
        { json: "type", js: "type", typ: u(undefined, r("ActionDefinitionType")) },
    ], "any"),
    "InputArgument": o([
        { json: "name", js: "name", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], "any"),
    "Header": o([
        { json: "name", js: "name", typ: "" },
        { json: "value", js: "value", typ: u(undefined, "") },
        { json: "value_from_env", js: "value_from_env", typ: u(undefined, "") },
    ], "any"),
    "Permissions": o([
        { json: "role", js: "role", typ: "" },
    ], "any"),
    "AllowList": o([
        { json: "collection", js: "collection", typ: "" },
    ], "any"),
    "CronTrigger": o([
        { json: "comment", js: "comment", typ: u(undefined, "") },
        { json: "headers", js: "headers", typ: a(r("Header")) },
        { json: "include_in_metadata", js: "include_in_metadata", typ: true },
        { json: "name", js: "name", typ: "" },
        { json: "payload", js: "payload", typ: u(undefined, m("any")) },
        { json: "retry_conf", js: "retry_conf", typ: u(undefined, r("RetryConfST")) },
        { json: "schedule", js: "schedule", typ: "" },
        { json: "webhook", js: "webhook", typ: "" },
    ], "any"),
    "RetryConfST": o([
        { json: "num_retries", js: "num_retries", typ: u(undefined, 0) },
        { json: "retry_interval_seconds", js: "retry_interval_seconds", typ: u(undefined, 0) },
        { json: "timeout_seconds", js: "timeout_seconds", typ: u(undefined, 0) },
        { json: "tolerance_seconds", js: "tolerance_seconds", typ: u(undefined, 0) },
    ], "any"),
    "CustomTypes": o([
        { json: "enums", js: "enums", typ: u(undefined, a(r("EnumType"))) },
        { json: "input_objects", js: "input_objects", typ: u(undefined, a(r("InputObjectType"))) },
        { json: "objects", js: "objects", typ: u(undefined, a(r("ObjectType"))) },
        { json: "scalars", js: "scalars", typ: u(undefined, a(r("ScalarType"))) },
    ], "any"),
    "EnumType": o([
        { json: "description", js: "description", typ: u(undefined, "") },
        { json: "name", js: "name", typ: "" },
        { json: "values", js: "values", typ: a(r("EnumValue")) },
    ], "any"),
    "EnumValue": o([
        { json: "description", js: "description", typ: u(undefined, "") },
        { json: "is_deprecated", js: "is_deprecated", typ: u(undefined, true) },
        { json: "value", js: "value", typ: "" },
    ], "any"),
    "InputObjectType": o([
        { json: "description", js: "description", typ: u(undefined, "") },
        { json: "fields", js: "fields", typ: a(r("InputObjectField")) },
        { json: "name", js: "name", typ: "" },
    ], "any"),
    "InputObjectField": o([
        { json: "description", js: "description", typ: u(undefined, "") },
        { json: "name", js: "name", typ: "" },
        { json: "type", js: "type", typ: "" },
    ], "any"),
    "ObjectType": o([
        { json: "description", js: "description", typ: u(undefined, "") },
        { json: "fields", js: "fields", typ: a(r("InputObjectField")) },
        { json: "name", js: "name", typ: "" },
        { json: "relationships", js: "relationships", typ: u(undefined, a(r("CustomTypeObjectRelationship"))) },
    ], "any"),
    "CustomTypeObjectRelationship": o([
        { json: "field_mapping", js: "field_mapping", typ: m("") },
        { json: "name", js: "name", typ: "" },
        { json: "remote_table", js: "remote_table", typ: u(r("QualifiedTable"), "") },
        { json: "type", js: "type", typ: r("CustomTypeObjectRelationshipType") },
    ], "any"),
    "QualifiedTable": o([
        { json: "name", js: "name", typ: "" },
        { json: "schema", js: "schema", typ: "" },
    ], "any"),
    "ScalarType": o([
        { json: "description", js: "description", typ: u(undefined, "") },
        { json: "name", js: "name", typ: "" },
    ], "any"),
    "CustomFunction": o([
        { json: "configuration", js: "configuration", typ: u(undefined, r("FunctionConfiguration")) },
        { json: "function", js: "function", typ: u(r("QualifiedFunction"), "") },
    ], "any"),
    "FunctionConfiguration": o([
        { json: "session_argument", js: "session_argument", typ: u(undefined, "") },
    ], "any"),
    "QualifiedFunction": o([
        { json: "name", js: "name", typ: "" },
        { json: "schema", js: "schema", typ: "" },
    ], "any"),
    "QueryCollectionEntry": o([
        { json: "comment", js: "comment", typ: u(undefined, "") },
        { json: "definition", js: "definition", typ: r("Definition") },
        { json: "name", js: "name", typ: "" },
    ], "any"),
    "Definition": o([
        { json: "queries", js: "queries", typ: a(r("QueryCollection")) },
    ], "any"),
    "QueryCollection": o([
        { json: "name", js: "name", typ: "" },
        { json: "query", js: "query", typ: "" },
    ], "any"),
    "RemoteSchema": o([
        { json: "comment", js: "comment", typ: u(undefined, "") },
        { json: "definition", js: "definition", typ: r("RemoteSchemaDef") },
        { json: "name", js: "name", typ: "" },
    ], "any"),
    "RemoteSchemaDef": o([
        { json: "forward_client_headers", js: "forward_client_headers", typ: u(undefined, true) },
        { json: "headers", js: "headers", typ: u(undefined, a(r("Header"))) },
        { json: "timeout_seconds", js: "timeout_seconds", typ: u(undefined, 3.14) },
        { json: "url", js: "url", typ: u(undefined, "") },
        { json: "url_from_env", js: "url_from_env", typ: u(undefined, "") },
    ], "any"),
    "TableEntry": o([
        { json: "array_relationships", js: "array_relationships", typ: u(undefined, a(r("ArrayRelationship"))) },
        { json: "computed_fields", js: "computed_fields", typ: u(undefined, a(r("ComputedField"))) },
        { json: "configuration", js: "configuration", typ: u(undefined, r("TableConfig")) },
        { json: "delete_permissions", js: "delete_permissions", typ: u(undefined, a(r("DeletePermissionEntry"))) },
        { json: "event_triggers", js: "event_triggers", typ: u(undefined, a(r("EventTrigger"))) },
        { json: "insert_permissions", js: "insert_permissions", typ: u(undefined, a(r("InsertPermissionEntry"))) },
        { json: "is_enum", js: "is_enum", typ: u(undefined, true) },
        { json: "object_relationships", js: "object_relationships", typ: u(undefined, a(r("ObjectRelationship"))) },
        { json: "remote_relationships", js: "remote_relationships", typ: u(undefined, a(r("RemoteRelationship"))) },
        { json: "select_permissions", js: "select_permissions", typ: u(undefined, a(r("SelectPermissionEntry"))) },
        { json: "table", js: "table", typ: r("QualifiedTable") },
        { json: "update_permissions", js: "update_permissions", typ: u(undefined, a(r("UpdatePermissionEntry"))) },
    ], "any"),
    "ArrayRelationship": o([
        { json: "comment", js: "comment", typ: u(undefined, "") },
        { json: "name", js: "name", typ: "" },
        { json: "using", js: "using", typ: r("ArrRelUsing") },
    ], "any"),
    "ArrRelUsing": o([
        { json: "foreign_key_constraint_on", js: "foreign_key_constraint_on", typ: u(undefined, r("ArrRelUsingFKeyOn")) },
        { json: "manual_configuration", js: "manual_configuration", typ: u(undefined, r("ArrRelUsingManualMapping")) },
    ], "any"),
    "ArrRelUsingFKeyOn": o([
        { json: "column", js: "column", typ: "" },
        { json: "table", js: "table", typ: u(r("QualifiedTable"), "") },
    ], "any"),
    "ArrRelUsingManualMapping": o([
        { json: "column_mapping", js: "column_mapping", typ: m("") },
        { json: "remote_table", js: "remote_table", typ: u(r("QualifiedTable"), "") },
    ], "any"),
    "ComputedField": o([
        { json: "comment", js: "comment", typ: u(undefined, "") },
        { json: "definition", js: "definition", typ: r("ComputedFieldDefinition") },
        { json: "name", js: "name", typ: "" },
    ], "any"),
    "ComputedFieldDefinition": o([
        { json: "function", js: "function", typ: u(r("QualifiedFunction"), "") },
        { json: "session_argument", js: "session_argument", typ: u(undefined, "") },
        { json: "table_argument", js: "table_argument", typ: u(undefined, "") },
    ], "any"),
    "TableConfig": o([
        { json: "custom_column_names", js: "custom_column_names", typ: u(undefined, m("")) },
        { json: "custom_root_fields", js: "custom_root_fields", typ: u(undefined, r("CustomRootFields")) },
    ], "any"),
    "CustomRootFields": o([
        { json: "delete", js: "delete", typ: u(undefined, "") },
        { json: "delete_by_pk", js: "delete_by_pk", typ: u(undefined, "") },
        { json: "insert", js: "insert", typ: u(undefined, "") },
        { json: "insert_one", js: "insert_one", typ: u(undefined, "") },
        { json: "select", js: "select", typ: u(undefined, "") },
        { json: "select_aggregate", js: "select_aggregate", typ: u(undefined, "") },
        { json: "select_by_pk", js: "select_by_pk", typ: u(undefined, "") },
        { json: "update", js: "update", typ: u(undefined, "") },
        { json: "update_by_pk", js: "update_by_pk", typ: u(undefined, "") },
    ], "any"),
    "DeletePermissionEntry": o([
        { json: "comment", js: "comment", typ: u(undefined, "") },
        { json: "permission", js: "permission", typ: r("DeletePermission") },
        { json: "role", js: "role", typ: "" },
    ], "any"),
    "DeletePermission": o([
        { json: "filter", js: "filter", typ: u(undefined, m(u(3.14, m("any"), ""))) },
    ], "any"),
    "EventTrigger": o([
        { json: "definition", js: "definition", typ: r("EventTriggerDefinition") },
        { json: "headers", js: "headers", typ: u(undefined, a(r("Header"))) },
        { json: "name", js: "name", typ: "" },
        { json: "retry_conf", js: "retry_conf", typ: r("RetryConf") },
        { json: "webhook", js: "webhook", typ: u(undefined, "") },
        { json: "webhook_from_env", js: "webhook_from_env", typ: u(undefined, "") },
    ], "any"),
    "EventTriggerDefinition": o([
        { json: "delete", js: "delete", typ: u(undefined, r("OperationSpec")) },
        { json: "enable_manual", js: "enable_manual", typ: true },
        { json: "insert", js: "insert", typ: u(undefined, r("OperationSpec")) },
        { json: "update", js: "update", typ: u(undefined, r("OperationSpec")) },
    ], "any"),
    "OperationSpec": o([
        { json: "columns", js: "columns", typ: u(a(""), r("Columns")) },
        { json: "payload", js: "payload", typ: u(undefined, u(a(""), r("Columns"))) },
    ], "any"),
    "RetryConf": o([
        { json: "interval_sec", js: "interval_sec", typ: u(undefined, 0) },
        { json: "num_retries", js: "num_retries", typ: u(undefined, 0) },
        { json: "timeout_sec", js: "timeout_sec", typ: u(undefined, 0) },
    ], "any"),
    "InsertPermissionEntry": o([
        { json: "comment", js: "comment", typ: u(undefined, "") },
        { json: "permission", js: "permission", typ: r("InsertPermission") },
        { json: "role", js: "role", typ: "" },
    ], "any"),
    "InsertPermission": o([
        { json: "backend_only", js: "backend_only", typ: u(undefined, true) },
        { json: "check", js: "check", typ: u(undefined, m(u(3.14, m("any"), ""))) },
        { json: "columns", js: "columns", typ: u(a(""), r("Columns")) },
        { json: "set", js: "set", typ: u(undefined, m("")) },
    ], "any"),
    "ObjectRelationship": o([
        { json: "comment", js: "comment", typ: u(undefined, "") },
        { json: "name", js: "name", typ: "" },
        { json: "using", js: "using", typ: r("ObjRelUsing") },
    ], "any"),
    "ObjRelUsing": o([
        { json: "foreign_key_constraint_on", js: "foreign_key_constraint_on", typ: u(undefined, "") },
        { json: "manual_configuration", js: "manual_configuration", typ: u(undefined, r("ObjRelUsingManualMapping")) },
    ], "any"),
    "ObjRelUsingManualMapping": o([
        { json: "column_mapping", js: "column_mapping", typ: m("") },
        { json: "remote_table", js: "remote_table", typ: u(r("QualifiedTable"), "") },
    ], "any"),
    "RemoteRelationship": o([
        { json: "definition", js: "definition", typ: r("RemoteRelationshipDef") },
        { json: "name", js: "name", typ: "" },
    ], "any"),
    "RemoteRelationshipDef": o([
        { json: "hasura_fields", js: "hasura_fields", typ: a("") },
        { json: "remote_field", js: "remote_field", typ: m(r("RemoteField")) },
        { json: "remote_schema", js: "remote_schema", typ: "" },
    ], "any"),
    "RemoteField": o([
        { json: "arguments", js: "arguments", typ: m("") },
        { json: "field", js: "field", typ: u(undefined, m(r("RemoteField"))) },
    ], "any"),
    "SelectPermissionEntry": o([
        { json: "comment", js: "comment", typ: u(undefined, "") },
        { json: "permission", js: "permission", typ: r("SelectPermission") },
        { json: "role", js: "role", typ: "" },
    ], "any"),
    "SelectPermission": o([
        { json: "allow_aggregations", js: "allow_aggregations", typ: u(undefined, true) },
        { json: "columns", js: "columns", typ: u(a(""), r("Columns")) },
        { json: "computed_fields", js: "computed_fields", typ: u(undefined, a("")) },
        { json: "filter", js: "filter", typ: u(undefined, m(u(3.14, m("any"), ""))) },
        { json: "limit", js: "limit", typ: u(undefined, 0) },
    ], "any"),
    "UpdatePermissionEntry": o([
        { json: "comment", js: "comment", typ: u(undefined, "") },
        { json: "permission", js: "permission", typ: r("UpdatePermission") },
        { json: "role", js: "role", typ: "" },
    ], "any"),
    "UpdatePermission": o([
        { json: "check", js: "check", typ: u(undefined, m(u(3.14, m("any"), ""))) },
        { json: "columns", js: "columns", typ: u(a(""), r("Columns")) },
        { json: "filter", js: "filter", typ: u(undefined, m(u(3.14, m("any"), ""))) },
        { json: "set", js: "set", typ: u(undefined, m("")) },
    ], "any"),
    "ActionDefinitionType": [
        "mutation",
        "query",
    ],
    "CustomTypeObjectRelationshipType": [
        "array",
        "object",
    ],
    "Columns": [
        "*",
    ],
};
