{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickType
    ( PGColumn (..)
    , ComputedFieldName (..)
    , RoleName (..)
    , TriggerName (..)
    , RemoteRelationshipName (..)
    , RemoteSchemaName (..)
    , CollectionName (..)
    , GraphQLName (..)
    , GraphQLType (..)
    , RelationshipName (..)
    , ActionName (..)
    , WebhookURL (..)
    , TableName (..)
    , QualifiedTable (..)
    , TableConfig (..)
    , TableEntry (..)
    , CustomRootFields (..)
    , CustomColumnNames (..)
    , FunctionName (..)
    , QualifiedFunction (..)
    , CustomFunction (..)
    , FunctionConfiguration (..)
    , ObjectRelationship (..)
    , ObjRelUsing (..)
    , ObjRelUsingManualMapping (..)
    , ArrayRelationship (..)
    , ArrRelUsing (..)
    , ArrRelUsingFKeyOn (..)
    , ArrRelUsingManualMapping (..)
    , ColumnPresetsExpression (..)
    , InsertPermissionEntry (..)
    , InsertPermission (..)
    , SelectPermissionEntry (..)
    , SelectPermission (..)
    , UpdatePermissionEntry (..)
    , UpdatePermission (..)
    , DeletePermissionEntry (..)
    , DeletePermission (..)
    , ComputedField (..)
    , ComputedFieldDefinition (..)
    , EventTrigger (..)
    , EventTriggerDefinition (..)
    , EventTriggerColumns (..)
    , OperationSpec (..)
    , HeaderFromValue (..)
    , HeaderFromEnv (..)
    , RetryConf (..)
    , CronTrigger (..)
    , RetryConfST (..)
    , RemoteSchema (..)
    , RemoteSchemaDef (..)
    , RemoteRelationship (..)
    , RemoteRelationshipDef (..)
    , RemoteField (..)
    , InputArguments (..)
    , QueryCollectionEntry (..)
    , QueryCollection (..)
    , AllowList (..)
    , CustomTypes (..)
    , InputObjectType (..)
    , InputObjectField (..)
    , ObjectType (..)
    , ObjectField (..)
    , CustomTypeObjectRelationship (..)
    , ScalarType (..)
    , EnumType (..)
    , EnumValue (..)
    , Action (..)
    , ActionDefinition (..)
    , InputArgument (..)
    , HasuraMetadataV2 (..)
    , Header (..)
    , Permissions (..)
    , Definition (..)
    , RemoteFieldValue (..)
    , ActionDefinitionType (..)
    , CustomTypeObjectRelationshipType (..)
    , Columns (..)
    , Filter (..)
    , decodeTopLevel
    ) where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Vector (Vector)

type PGColumn = Text

type ComputedFieldName = Text

type RoleName = Text

type TriggerName = Text

type RemoteRelationshipName = Text

type RemoteSchemaName = Text

type CollectionName = Text

type GraphQLName = Text

type GraphQLType = Text

type RelationshipName = Text

type ActionName = Text

type WebhookURL = Text

type CustomColumnNames = HashMap Text Text

type ColumnPresetsExpression = HashMap Text Text

type RemoteField = HashMap Text RemoteFieldValue

type InputArguments = HashMap Text Text

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromvalue

name:
Name of the header

value:
Value of the header
-}
data HeaderFromValue = HeaderFromValue
    { nameHeaderFromValue :: Text
    , valueHeaderFromValue :: Text
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromenv

name:
Name of the header

valueFromEnv:
Name of the environment variable which holds the value of the header
-}
data HeaderFromEnv = HeaderFromEnv
    { nameHeaderFromEnv :: Text
    , valueFromEnvHeaderFromEnv :: Text
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#objectfield

description:
Description of the Input object type

name:
Name of the Input object type

objectFieldType:
GraphQL type of the Input object type
-}
data ObjectField = ObjectField
    { descriptionObjectField :: Maybe Text
    , nameObjectField :: Text
    , objectFieldTypeObjectField :: Text
    } deriving (Show)

{-| Type used in exported 'metadata.json' and replace metadata endpoint

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/manage-metadata.html#replace-metadata
-}
data HasuraMetadataV2 = HasuraMetadataV2
    { actionsHasuraMetadataV2 :: Maybe (Vector Action)
    , allowlistHasuraMetadataV2 :: Maybe (Vector AllowList)
    , cronTriggersHasuraMetadataV2 :: Maybe (Vector CronTrigger)
    , customTypesHasuraMetadataV2 :: Maybe CustomTypes
    , functionsHasuraMetadataV2 :: Maybe (Vector CustomFunction)
    , queryCollectionsHasuraMetadataV2 :: Maybe (Vector QueryCollectionEntry)
    , remoteSchemasHasuraMetadataV2 :: Maybe (Vector RemoteSchema)
    , tablesHasuraMetadataV2 :: Vector TableEntry
    , versionHasuraMetadataV2 :: Float
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/actions.html#args-syntax

comment:
Comment

definition:
Definition of the action

name:
Name of the action

permissions:
Permissions of the action
-}
data Action = Action
    { commentAction :: Maybe Text
    , definitionAction :: ActionDefinition
    , nameAction :: Text
    , permissionsAction :: Maybe Permissions
    } deriving (Show)

{-| Definition of the action


https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/actions.html#actiondefinition

handler:
A String value which supports templating environment variables enclosed in {{ and }}.
Template example: https://{{ACTION_API_DOMAIN}}/create-user
-}
data ActionDefinition = ActionDefinition
    { argumentsActionDefinition :: Maybe (Vector InputArgument)
    , forwardClientHeadersActionDefinition :: Maybe Bool
    , handlerActionDefinition :: Text
    , headersActionDefinition :: Maybe (Vector Header)
    , kindActionDefinition :: Maybe Text
    , outputTypeActionDefinition :: Maybe Text
    , actionDefinitionTypeActionDefinition :: Maybe ActionDefinitionType
    } deriving (Show)

data ActionDefinitionType
    = MutationActionDefinitionType
    | QueryActionDefinitionType
    deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/actions.html#inputargument
-}
data InputArgument = InputArgument
    { nameInputArgument :: Text
    , inputArgumentTypeInputArgument :: Text
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromvalue


https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromenv

name:
Name of the header

value:
Value of the header

valueFromEnv:
Name of the environment variable which holds the value of the header
-}
data Header = Header
    { nameHeader :: Text
    , valueHeader :: Maybe Text
    , valueFromEnvHeader :: Maybe Text
    } deriving (Show)

{-| Permissions of the action -}
data Permissions = Permissions
    { rolePermissions :: Text
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/query-collections.html#add-collection-to-allowlist-syntax

collection:
Name of a query collection to be added to the allow-list
-}
data AllowList = AllowList
    { collectionAllowList :: Text
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/scheduled-triggers.html#create-cron-trigger

comment:
Custom comment.

headers:
List of headers to be sent with the webhook

includeInMetadata:
Flag to indicate whether a trigger should be included in the metadata. When a cron
trigger is included in the metadata, the user will be able to export it when the metadata
of the graphql-engine is exported.

name:
Name of the cron trigger

payload:
Any JSON payload which will be sent when the webhook is invoked.

retryConf:
Retry configuration if scheduled invocation delivery fails

schedule:
Cron expression at which the trigger should be invoked.

webhook:
URL of the webhook
-}
data CronTrigger = CronTrigger
    { commentCronTrigger :: Maybe Text
    , headersCronTrigger :: Vector Header
    , includeInMetadataCronTrigger :: Bool
    , nameCronTrigger :: Text
    , payloadCronTrigger :: Maybe (HashMap Text (Maybe Text))
    , retryConfCronTrigger :: Maybe RetryConfST
    , scheduleCronTrigger :: Text
    , webhookCronTrigger :: Text
    } deriving (Show)

{-| Retry configuration if scheduled invocation delivery fails


https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/scheduled-triggers.html#retryconfst

numRetries:
Number of times to retry delivery.
Default: 0

retryIntervalSeconds:
Number of seconds to wait between each retry.
Default: 10

timeoutSeconds:
Number of seconds to wait for response before timing out.
Default: 60

toleranceSeconds:
Number of seconds between scheduled time and actual delivery time that is acceptable. If
the time difference is more than this, then the event is dropped.
Default: 21600 (6 hours)
-}
data RetryConfST = RetryConfST
    { numRetriesRetryConfST :: Maybe Int
    , retryIntervalSecondsRetryConfST :: Maybe Int
    , timeoutSecondsRetryConfST :: Maybe Int
    , toleranceSecondsRetryConfST :: Maybe Int
    } deriving (Show)

data CustomTypes = CustomTypes
    { enumsCustomTypes :: Maybe (Vector EnumType)
    , inputObjectsCustomTypes :: Maybe (Vector InputObjectType)
    , objectsCustomTypes :: Maybe (Vector ObjectType)
    , scalarsCustomTypes :: Maybe (Vector ScalarType)
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#enumtype

description:
Description of the Enum type

name:
Name of the Enum type

values:
Values of the Enum type
-}
data EnumType = EnumType
    { descriptionEnumType :: Maybe Text
    , nameEnumType :: Text
    , valuesEnumType :: Vector EnumValue
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#enumvalue

description:
Description of the Enum value

isDeprecated:
If set to true, the enum value is marked as deprecated

value:
Value of the Enum type
-}
data EnumValue = EnumValue
    { descriptionEnumValue :: Maybe Text
    , isDeprecatedEnumValue :: Maybe Bool
    , valueEnumValue :: Text
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#inputobjecttype

description:
Description of the Input object type

fields:
Fields of the Input object type

name:
Name of the Input object type
-}
data InputObjectType = InputObjectType
    { descriptionInputObjectType :: Maybe Text
    , fieldsInputObjectType :: Vector InputObjectField
    , nameInputObjectType :: Text
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#inputobjectfield

description:
Description of the Input object type

name:
Name of the Input object type

inputObjectFieldType:
GraphQL type of the Input object type
-}
data InputObjectField = InputObjectField
    { descriptionInputObjectField :: Maybe Text
    , nameInputObjectField :: Text
    , inputObjectFieldTypeInputObjectField :: Text
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#objecttype

description:
Description of the Input object type

fields:
Fields of the Input object type

name:
Name of the Input object type

relationships:
Relationships of the Object type to tables
-}
data ObjectType = ObjectType
    { descriptionObjectType :: Maybe Text
    , fieldsObjectType :: Vector InputObjectField
    , nameObjectType :: Text
    , relationshipsObjectType :: Maybe (Vector CustomTypeObjectRelationship)
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#objectrelationship

fieldMapping:
Mapping of fields of object type to columns of remote table

name:
Name of the relationship, shouldn’t conflict with existing field names

remoteTable:
The table to which relationship is defined

customTypeObjectRelationshipType:
Type of the relationship
-}
data CustomTypeObjectRelationship = CustomTypeObjectRelationship
    { fieldMappingCustomTypeObjectRelationship :: HashMap Text Text
    , nameCustomTypeObjectRelationship :: Text
    , remoteTableCustomTypeObjectRelationship :: TableName
    , customTypeObjectRelationshipTypeCustomTypeObjectRelationship :: CustomTypeObjectRelationshipType
    } deriving (Show)

{-| Type of the relationship -}
data CustomTypeObjectRelationshipType
    = TypeArrayCustomTypeObjectRelationshipType
    | TypeObjectCustomTypeObjectRelationshipType
    deriving (Show)

data TableName
    = QualifiedTableInTableName QualifiedTable
    | StringInTableName Text
    deriving (Show)

data QualifiedTable = QualifiedTable
    { nameQualifiedTable :: Text
    , schemaQualifiedTable :: Text
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#scalartype

description:
Description of the Scalar type

name:
Name of the Scalar type
-}
data ScalarType = ScalarType
    { descriptionScalarType :: Maybe Text
    , nameScalarType :: Text
    } deriving (Show)

{-| A custom SQL function to add to the GraphQL schema with configuration.

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-functions.html#args-syntax

configuration:
Configuration for the SQL function

function:
Name of the SQL function
-}
data CustomFunction = CustomFunction
    { configurationCustomFunction :: Maybe FunctionConfiguration
    , functionCustomFunction :: FunctionName
    } deriving (Show)

{-| Configuration for the SQL function

Configuration for a CustomFunction

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-functions.html#function-configuration

sessionArgument:
Function argument which accepts session info JSON
Currently, only functions which satisfy the following constraints can be exposed over the
GraphQL API (terminology from Postgres docs):
- Function behaviour: ONLY `STABLE` or `IMMUTABLE`
- Return type: MUST be `SETOF <table-name>`
- Argument modes: ONLY `IN`
-}
data FunctionConfiguration = FunctionConfiguration
    { sessionArgumentFunctionConfiguration :: Maybe Text
    } deriving (Show)

data FunctionName
    = QualifiedFunctionInFunctionName QualifiedFunction
    | StringInFunctionName Text
    deriving (Show)

data QualifiedFunction = QualifiedFunction
    { nameQualifiedFunction :: Text
    , schemaQualifiedFunction :: Text
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/query-collections.html#args-syntax

comment:
Comment

definition:
List of queries

name:
Name of the query collection
-}
data QueryCollectionEntry = QueryCollectionEntry
    { commentQueryCollectionEntry :: Maybe Text
    , definitionQueryCollectionEntry :: Definition
    , nameQueryCollectionEntry :: Text
    } deriving (Show)

{-| List of queries -}
data Definition = Definition
    { queriesDefinition :: Vector QueryCollection
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#collectionquery
-}
data QueryCollection = QueryCollection
    { nameQueryCollection :: Text
    , queryQueryCollection :: Text
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/remote-schemas.html#add-remote-schema

comment:
Comment

definition:
Name of the remote schema

name:
Name of the remote schema
-}
data RemoteSchema = RemoteSchema
    { commentRemoteSchema :: Maybe Text
    , definitionRemoteSchema :: RemoteSchemaDef
    , nameRemoteSchema :: Text
    } deriving (Show)

{-| Name of the remote schema


https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#remoteschemadef
-}
data RemoteSchemaDef = RemoteSchemaDef
    { forwardClientHeadersRemoteSchemaDef :: Maybe Bool
    , headersRemoteSchemaDef :: Maybe (Vector Header)
    , timeoutSecondsRemoteSchemaDef :: Maybe Float
    , urlRemoteSchemaDef :: Maybe Text
    , urlFromEnvRemoteSchemaDef :: Maybe Text
    } deriving (Show)

{-| Representation of a table in metadata, 'tables.yaml' and 'metadata.json'

configuration:
Configuration for the table/view

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/table-view.html#table-config
-}
data TableEntry = TableEntry
    { arrayRelationshipsTableEntry :: Maybe (Vector ArrayRelationship)
    , computedFieldsTableEntry :: Maybe (Vector ComputedField)
    , configurationTableEntry :: Maybe TableConfig
    , deletePermissionsTableEntry :: Maybe (Vector DeletePermissionEntry)
    , eventTriggersTableEntry :: Maybe (Vector EventTrigger)
    , insertPermissionsTableEntry :: Maybe (Vector InsertPermissionEntry)
    , isEnumTableEntry :: Maybe Bool
    , objectRelationshipsTableEntry :: Maybe (Vector ObjectRelationship)
    , remoteRelationshipsTableEntry :: Maybe (Vector RemoteRelationship)
    , selectPermissionsTableEntry :: Maybe (Vector SelectPermissionEntry)
    , tableTableEntry :: QualifiedTable
    , updatePermissionsTableEntry :: Maybe (Vector UpdatePermissionEntry)
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#create-array-relationship-syntax

comment:
Comment

name:
Name of the new relationship

using:
Use one of the available ways to define an array relationship
-}
data ArrayRelationship = ArrayRelationship
    { commentArrayRelationship :: Maybe Text
    , nameArrayRelationship :: Text
    , usingArrayRelationship :: ArrRelUsing
    } deriving (Show)

{-| Use one of the available ways to define an array relationship

Use one of the available ways to define an object relationship

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#arrrelusing

foreignKeyConstraintOn:
The column with foreign key constraint

manualConfiguration:
Manual mapping of table and columns
-}
data ArrRelUsing = ArrRelUsing
    { foreignKeyConstraintOnArrRelUsing :: Maybe ArrRelUsingFKeyOn
    , manualConfigurationArrRelUsing :: Maybe ArrRelUsingManualMapping
    } deriving (Show)

{-| The column with foreign key constraint

The column with foreign key constraint

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#arrrelusingfkeyon
-}
data ArrRelUsingFKeyOn = ArrRelUsingFKeyOn
    { columnArrRelUsingFKeyOn :: Text
    , tableArrRelUsingFKeyOn :: TableName
    } deriving (Show)

{-| Manual mapping of table and columns

Manual mapping of table and columns

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#arrrelusingmanualmapping

columnMapping:
Mapping of columns from current table to remote table

remoteTable:
The table to which the relationship has to be established
-}
data ArrRelUsingManualMapping = ArrRelUsingManualMapping
    { columnMappingArrRelUsingManualMapping :: HashMap Text Text
    , remoteTableArrRelUsingManualMapping :: TableName
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/computed-field.html#args-syntax

comment:
Comment

definition:
The computed field definition

name:
Name of the new computed field
-}
data ComputedField = ComputedField
    { commentComputedField :: Maybe Text
    , definitionComputedField :: ComputedFieldDefinition
    , nameComputedField :: Text
    } deriving (Show)

{-| The computed field definition


https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/computed-field.html#computedfielddefinition

function:
The SQL function

sessionArgument:
Name of the argument which accepts the Hasura session object as a JSON/JSONB value. If
omitted, the Hasura session object is not passed to the function

tableArgument:
Name of the argument which accepts a table row type. If omitted, the first argument is
considered a table argument
-}
data ComputedFieldDefinition = ComputedFieldDefinition
    { functionComputedFieldDefinition :: FunctionName
    , sessionArgumentComputedFieldDefinition :: Maybe Text
    , tableArgumentComputedFieldDefinition :: Maybe Text
    } deriving (Show)

{-| Configuration for the table/view

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/table-view.html#table-config

customColumnNames:
Customise the column names

customRootFields:
Customise the root fields
-}
data TableConfig = TableConfig
    { customColumnNamesTableConfig :: Maybe (HashMap Text Text)
    , customRootFieldsTableConfig :: Maybe CustomRootFields
    } deriving (Show)

{-| Customise the root fields

Customise the root fields

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/table-view.html#custom-root-fields

delete:
Customise the `delete_<table-name>` root field

deleteByPk:
Customise the `delete_<table-name>_by_pk` root field

insert:
Customise the `insert_<table-name>` root field

insertOne:
Customise the `insert_<table-name>_one` root field

select:
Customise the `<table-name>` root field

selectAggregate:
Customise the `<table-name>_aggregate` root field

selectByPk:
Customise the `<table-name>_by_pk` root field

update:
Customise the `update_<table-name>` root field

updateByPk:
Customise the `update_<table-name>_by_pk` root field
-}
data CustomRootFields = CustomRootFields
    { deleteCustomRootFields :: Maybe Text
    , deleteByPkCustomRootFields :: Maybe Text
    , insertCustomRootFields :: Maybe Text
    , insertOneCustomRootFields :: Maybe Text
    , selectCustomRootFields :: Maybe Text
    , selectAggregateCustomRootFields :: Maybe Text
    , selectByPkCustomRootFields :: Maybe Text
    , updateCustomRootFields :: Maybe Text
    , updateByPkCustomRootFields :: Maybe Text
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#create-delete-permission-syntax

comment:
Comment

permission:
The permission definition

role:
Role
-}
data DeletePermissionEntry = DeletePermissionEntry
    { commentDeletePermissionEntry :: Maybe Text
    , permissionDeletePermissionEntry :: DeletePermission
    , roleDeletePermissionEntry :: Text
    } deriving (Show)

{-| The permission definition


https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#deletepermission

filter:
Only the rows where this precondition holds true are updatable
-}
data DeletePermission = DeletePermission
    { filterDeletePermission :: Maybe (HashMap Text Filter)
    } deriving (Show)

data Filter
    = AnythingMapInFilter (HashMap Text (Maybe Text))
    | DoubleInFilter Float
    | StringInFilter Text
    deriving (Show)

{-| NOTE: The metadata type doesn't QUITE match the 'create' arguments here

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#create-event-trigger

definition:
The SQL function

headers:
The SQL function

name:
Name of the event trigger

retryConf:
The SQL function

webhook:
The SQL function
-}
data EventTrigger = EventTrigger
    { definitionEventTrigger :: EventTriggerDefinition
    , headersEventTrigger :: Maybe (Vector Header)
    , nameEventTrigger :: Text
    , retryConfEventTrigger :: RetryConf
    , webhookEventTrigger :: Maybe Text
    , webhookFromEnvEventTrigger :: Maybe Text
    } deriving (Show)

{-| The SQL function

delete:

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec

insert:

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec

update:

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec
-}
data EventTriggerDefinition = EventTriggerDefinition
    { deleteEventTriggerDefinition :: Maybe OperationSpec
    , enableManualEventTriggerDefinition :: Bool
    , insertEventTriggerDefinition :: Maybe OperationSpec
    , updateEventTriggerDefinition :: Maybe OperationSpec
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec

columns:

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#eventtriggercolumns

payload:

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#eventtriggercolumns
-}
data OperationSpec = OperationSpec
    { columnsOperationSpec :: EventTriggerColumns
    , payloadOperationSpec :: Maybe EventTriggerColumns
    } deriving (Show)

data EventTriggerColumns
    = EnumInEventTriggerColumns Columns
    | StringArrayInEventTriggerColumns (Vector Text)
    deriving (Show)

data Columns
    = EmptyColumns
    deriving (Show)

{-| The SQL function


https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#retryconf

intervalSEC:
Number of seconds to wait between each retry.
Default: 10

numRetries:
Number of times to retry delivery.
Default: 0

timeoutSEC:
Number of seconds to wait for response before timing out.
Default: 60
-}
data RetryConf = RetryConf
    { intervalSECRetryConf :: Maybe Int
    , numRetriesRetryConf :: Maybe Int
    , timeoutSECRetryConf :: Maybe Int
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#args-syntax

comment:
Comment

permission:
The permission definition

role:
Role
-}
data InsertPermissionEntry = InsertPermissionEntry
    { commentInsertPermissionEntry :: Maybe Text
    , permissionInsertPermissionEntry :: InsertPermission
    , roleInsertPermissionEntry :: Text
    } deriving (Show)

{-| The permission definition


https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#insertpermission

backendOnly:
When set to true the mutation is accessible only if x-hasura-use-backend-only-permissions
session variable exists
and is set to true and request is made with x-hasura-admin-secret set if any auth is
configured

check:
This expression has to hold true for every new row that is inserted

columns:
Can insert into only these columns (or all when '*' is specified)

set:
Preset values for columns that can be sourced from session variables or static values
-}
data InsertPermission = InsertPermission
    { backendOnlyInsertPermission :: Maybe Bool
    , checkInsertPermission :: Maybe (HashMap Text Filter)
    , columnsInsertPermission :: EventTriggerColumns
    , setInsertPermission :: Maybe (HashMap Text Text)
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#args-syntax

comment:
Comment

name:
Name of the new relationship

using:
Use one of the available ways to define an object relationship
-}
data ObjectRelationship = ObjectRelationship
    { commentObjectRelationship :: Maybe Text
    , nameObjectRelationship :: Text
    , usingObjectRelationship :: ObjRelUsing
    } deriving (Show)

{-| Use one of the available ways to define an object relationship

Use one of the available ways to define an object relationship

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#objrelusing

foreignKeyConstraintOn:
The column with foreign key constraint

manualConfiguration:
Manual mapping of table and columns
-}
data ObjRelUsing = ObjRelUsing
    { foreignKeyConstraintOnObjRelUsing :: Maybe Text
    , manualConfigurationObjRelUsing :: Maybe ObjRelUsingManualMapping
    } deriving (Show)

{-| Manual mapping of table and columns

Manual mapping of table and columns

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#objrelusingmanualmapping

columnMapping:
Mapping of columns from current table to remote table

remoteTable:
The table to which the relationship has to be established
-}
data ObjRelUsingManualMapping = ObjRelUsingManualMapping
    { columnMappingObjRelUsingManualMapping :: HashMap Text Text
    , remoteTableObjRelUsingManualMapping :: TableName
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/remote-relationships.html#args-syntax

definition:
Definition object

name:
Name of the remote relationship
-}
data RemoteRelationship = RemoteRelationship
    { definitionRemoteRelationship :: RemoteRelationshipDef
    , nameRemoteRelationship :: Text
    } deriving (Show)

{-| Definition object

hasuraFields:
Column(s) in the table that is used for joining with remote schema field.
All join keys in remote_field must appear here.

remoteField:
The schema tree ending at the field in remote schema which needs to be joined with.

remoteSchema:
Name of the remote schema to join with
-}
data RemoteRelationshipDef = RemoteRelationshipDef
    { hasuraFieldsRemoteRelationshipDef :: Vector Text
    , remoteFieldRemoteRelationshipDef :: HashMap Text RemoteFieldValue
    , remoteSchemaRemoteRelationshipDef :: Text
    } deriving (Show)

{-| field:
A recursive tree structure that points to the field in the remote schema that needs to be
joined with.
It is recursive because the remote field maybe nested deeply in the remote schema.

https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/remote-relationships.html#remotefield
-}
data RemoteFieldValue = RemoteFieldValue
    { argumentsRemoteFieldValue :: HashMap Text Text
    , fieldRemoteFieldValue :: Maybe (HashMap Text RemoteFieldValue)
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#create-select-permission-syntax

comment:
Comment

permission:
The permission definition

role:
Role
-}
data SelectPermissionEntry = SelectPermissionEntry
    { commentSelectPermissionEntry :: Maybe Text
    , permissionSelectPermissionEntry :: SelectPermission
    , roleSelectPermissionEntry :: Text
    } deriving (Show)

{-| The permission definition


https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#selectpermission

allowAggregations:
Toggle allowing aggregate queries

columns:
Only these columns are selectable (or all when '*' is specified)

computedFields:
Only these computed fields are selectable

filter:
Only the rows where this precondition holds true are selectable

limit:
The maximum number of rows that can be returned
-}
data SelectPermission = SelectPermission
    { allowAggregationsSelectPermission :: Maybe Bool
    , columnsSelectPermission :: EventTriggerColumns
    , computedFieldsSelectPermission :: Maybe (Vector Text)
    , filterSelectPermission :: Maybe (HashMap Text Filter)
    , limitSelectPermission :: Maybe Int
    } deriving (Show)

{-|
https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#create-update-permission-syntax

comment:
Comment

permission:
The permission definition

role:
Role
-}
data UpdatePermissionEntry = UpdatePermissionEntry
    { commentUpdatePermissionEntry :: Maybe Text
    , permissionUpdatePermissionEntry :: UpdatePermission
    , roleUpdatePermissionEntry :: Text
    } deriving (Show)

{-| The permission definition


https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#updatepermission

check:
Postcondition which must be satisfied by rows which have been updated

columns:
Only these columns are selectable (or all when '*' is specified)

filter:
Only the rows where this precondition holds true are updatable

set:
Preset values for columns that can be sourced from session variables or static values
-}
data UpdatePermission = UpdatePermission
    { checkUpdatePermission :: Maybe (HashMap Text Filter)
    , columnsUpdatePermission :: EventTriggerColumns
    , filterUpdatePermission :: Maybe (HashMap Text Filter)
    , setUpdatePermission :: Maybe (HashMap Text Text)
    } deriving (Show)

decodeTopLevel :: ByteString -> Maybe PGColumn
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ComputedFieldName
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe RoleName
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe TriggerName
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe RemoteRelationshipName
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe RemoteSchemaName
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe CollectionName
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe GraphQLName
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe GraphQLType
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe RelationshipName
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ActionName
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe WebhookURL
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe TableName
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe QualifiedTable
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe TableConfig
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe TableEntry
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe CustomRootFields
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe CustomColumnNames
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe FunctionName
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe QualifiedFunction
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe CustomFunction
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe FunctionConfiguration
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ObjectRelationship
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ObjRelUsing
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ObjRelUsingManualMapping
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ArrayRelationship
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ArrRelUsing
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ArrRelUsingFKeyOn
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ArrRelUsingManualMapping
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ColumnPresetsExpression
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe InsertPermissionEntry
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe InsertPermission
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe SelectPermissionEntry
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe SelectPermission
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe UpdatePermissionEntry
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe UpdatePermission
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe DeletePermissionEntry
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe DeletePermission
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ComputedField
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ComputedFieldDefinition
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe EventTrigger
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe EventTriggerDefinition
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe EventTriggerColumns
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe OperationSpec
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe HeaderFromValue
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe HeaderFromEnv
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe RetryConf
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe CronTrigger
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe RetryConfST
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe RemoteSchema
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe RemoteSchemaDef
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe RemoteRelationship
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe RemoteRelationshipDef
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe RemoteField
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe InputArguments
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe QueryCollectionEntry
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe QueryCollection
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe AllowList
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe CustomTypes
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe InputObjectType
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe InputObjectField
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ObjectType
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ObjectField
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe CustomTypeObjectRelationship
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ScalarType
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe EnumType
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe EnumValue
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe Action
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe ActionDefinition
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe InputArgument
decodeTopLevel = decode

decodeTopLevel :: ByteString -> Maybe HasuraMetadataV2
decodeTopLevel = decode

instance ToJSON HeaderFromValue where
    toJSON (HeaderFromValue nameHeaderFromValue valueHeaderFromValue) =
        object
        [ "name" .= nameHeaderFromValue
        , "value" .= valueHeaderFromValue
        ]

instance FromJSON HeaderFromValue where
    parseJSON (Object v) = HeaderFromValue
        <$> v .: "name"
        <*> v .: "value"

instance ToJSON HeaderFromEnv where
    toJSON (HeaderFromEnv nameHeaderFromEnv valueFromEnvHeaderFromEnv) =
        object
        [ "name" .= nameHeaderFromEnv
        , "value_from_env" .= valueFromEnvHeaderFromEnv
        ]

instance FromJSON HeaderFromEnv where
    parseJSON (Object v) = HeaderFromEnv
        <$> v .: "name"
        <*> v .: "value_from_env"

instance ToJSON ObjectField where
    toJSON (ObjectField descriptionObjectField nameObjectField objectFieldTypeObjectField) =
        object
        [ "description" .= descriptionObjectField
        , "name" .= nameObjectField
        , "type" .= objectFieldTypeObjectField
        ]

instance FromJSON ObjectField where
    parseJSON (Object v) = ObjectField
        <$> v .:? "description"
        <*> v .: "name"
        <*> v .: "type"

instance ToJSON HasuraMetadataV2 where
    toJSON (HasuraMetadataV2 actionsHasuraMetadataV2 allowlistHasuraMetadataV2 cronTriggersHasuraMetadataV2 customTypesHasuraMetadataV2 functionsHasuraMetadataV2 queryCollectionsHasuraMetadataV2 remoteSchemasHasuraMetadataV2 tablesHasuraMetadataV2 versionHasuraMetadataV2) =
        object
        [ "actions" .= actionsHasuraMetadataV2
        , "allowlist" .= allowlistHasuraMetadataV2
        , "cron_triggers" .= cronTriggersHasuraMetadataV2
        , "custom_types" .= customTypesHasuraMetadataV2
        , "functions" .= functionsHasuraMetadataV2
        , "query_collections" .= queryCollectionsHasuraMetadataV2
        , "remote_schemas" .= remoteSchemasHasuraMetadataV2
        , "tables" .= tablesHasuraMetadataV2
        , "version" .= versionHasuraMetadataV2
        ]

instance FromJSON HasuraMetadataV2 where
    parseJSON (Object v) = HasuraMetadataV2
        <$> v .:? "actions"
        <*> v .:? "allowlist"
        <*> v .:? "cron_triggers"
        <*> v .:? "custom_types"
        <*> v .:? "functions"
        <*> v .:? "query_collections"
        <*> v .:? "remote_schemas"
        <*> v .: "tables"
        <*> v .: "version"

instance ToJSON Action where
    toJSON (Action commentAction definitionAction nameAction permissionsAction) =
        object
        [ "comment" .= commentAction
        , "definition" .= definitionAction
        , "name" .= nameAction
        , "permissions" .= permissionsAction
        ]

instance FromJSON Action where
    parseJSON (Object v) = Action
        <$> v .:? "comment"
        <*> v .: "definition"
        <*> v .: "name"
        <*> v .:? "permissions"

instance ToJSON ActionDefinition where
    toJSON (ActionDefinition argumentsActionDefinition forwardClientHeadersActionDefinition handlerActionDefinition headersActionDefinition kindActionDefinition outputTypeActionDefinition actionDefinitionTypeActionDefinition) =
        object
        [ "arguments" .= argumentsActionDefinition
        , "forward_client_headers" .= forwardClientHeadersActionDefinition
        , "handler" .= handlerActionDefinition
        , "headers" .= headersActionDefinition
        , "kind" .= kindActionDefinition
        , "output_type" .= outputTypeActionDefinition
        , "type" .= actionDefinitionTypeActionDefinition
        ]

instance FromJSON ActionDefinition where
    parseJSON (Object v) = ActionDefinition
        <$> v .:? "arguments"
        <*> v .:? "forward_client_headers"
        <*> v .: "handler"
        <*> v .:? "headers"
        <*> v .:? "kind"
        <*> v .:? "output_type"
        <*> v .:? "type"

instance ToJSON ActionDefinitionType where
    toJSON MutationActionDefinitionType = "mutation"
    toJSON QueryActionDefinitionType = "query"

instance FromJSON ActionDefinitionType where
    parseJSON = withText "ActionDefinitionType" parseText
        where
            parseText "mutation" = return MutationActionDefinitionType
            parseText "query" = return QueryActionDefinitionType

instance ToJSON InputArgument where
    toJSON (InputArgument nameInputArgument inputArgumentTypeInputArgument) =
        object
        [ "name" .= nameInputArgument
        , "type" .= inputArgumentTypeInputArgument
        ]

instance FromJSON InputArgument where
    parseJSON (Object v) = InputArgument
        <$> v .: "name"
        <*> v .: "type"

instance ToJSON Header where
    toJSON (Header nameHeader valueHeader valueFromEnvHeader) =
        object
        [ "name" .= nameHeader
        , "value" .= valueHeader
        , "value_from_env" .= valueFromEnvHeader
        ]

instance FromJSON Header where
    parseJSON (Object v) = Header
        <$> v .: "name"
        <*> v .:? "value"
        <*> v .:? "value_from_env"

instance ToJSON Permissions where
    toJSON (Permissions rolePermissions) =
        object
        [ "role" .= rolePermissions
        ]

instance FromJSON Permissions where
    parseJSON (Object v) = Permissions
        <$> v .: "role"

instance ToJSON AllowList where
    toJSON (AllowList collectionAllowList) =
        object
        [ "collection" .= collectionAllowList
        ]

instance FromJSON AllowList where
    parseJSON (Object v) = AllowList
        <$> v .: "collection"

instance ToJSON CronTrigger where
    toJSON (CronTrigger commentCronTrigger headersCronTrigger includeInMetadataCronTrigger nameCronTrigger payloadCronTrigger retryConfCronTrigger scheduleCronTrigger webhookCronTrigger) =
        object
        [ "comment" .= commentCronTrigger
        , "headers" .= headersCronTrigger
        , "include_in_metadata" .= includeInMetadataCronTrigger
        , "name" .= nameCronTrigger
        , "payload" .= payloadCronTrigger
        , "retry_conf" .= retryConfCronTrigger
        , "schedule" .= scheduleCronTrigger
        , "webhook" .= webhookCronTrigger
        ]

instance FromJSON CronTrigger where
    parseJSON (Object v) = CronTrigger
        <$> v .:? "comment"
        <*> v .: "headers"
        <*> v .: "include_in_metadata"
        <*> v .: "name"
        <*> v .:? "payload"
        <*> v .:? "retry_conf"
        <*> v .: "schedule"
        <*> v .: "webhook"

instance ToJSON RetryConfST where
    toJSON (RetryConfST numRetriesRetryConfST retryIntervalSecondsRetryConfST timeoutSecondsRetryConfST toleranceSecondsRetryConfST) =
        object
        [ "num_retries" .= numRetriesRetryConfST
        , "retry_interval_seconds" .= retryIntervalSecondsRetryConfST
        , "timeout_seconds" .= timeoutSecondsRetryConfST
        , "tolerance_seconds" .= toleranceSecondsRetryConfST
        ]

instance FromJSON RetryConfST where
    parseJSON (Object v) = RetryConfST
        <$> v .:? "num_retries"
        <*> v .:? "retry_interval_seconds"
        <*> v .:? "timeout_seconds"
        <*> v .:? "tolerance_seconds"

instance ToJSON CustomTypes where
    toJSON (CustomTypes enumsCustomTypes inputObjectsCustomTypes objectsCustomTypes scalarsCustomTypes) =
        object
        [ "enums" .= enumsCustomTypes
        , "input_objects" .= inputObjectsCustomTypes
        , "objects" .= objectsCustomTypes
        , "scalars" .= scalarsCustomTypes
        ]

instance FromJSON CustomTypes where
    parseJSON (Object v) = CustomTypes
        <$> v .:? "enums"
        <*> v .:? "input_objects"
        <*> v .:? "objects"
        <*> v .:? "scalars"

instance ToJSON EnumType where
    toJSON (EnumType descriptionEnumType nameEnumType valuesEnumType) =
        object
        [ "description" .= descriptionEnumType
        , "name" .= nameEnumType
        , "values" .= valuesEnumType
        ]

instance FromJSON EnumType where
    parseJSON (Object v) = EnumType
        <$> v .:? "description"
        <*> v .: "name"
        <*> v .: "values"

instance ToJSON EnumValue where
    toJSON (EnumValue descriptionEnumValue isDeprecatedEnumValue valueEnumValue) =
        object
        [ "description" .= descriptionEnumValue
        , "is_deprecated" .= isDeprecatedEnumValue
        , "value" .= valueEnumValue
        ]

instance FromJSON EnumValue where
    parseJSON (Object v) = EnumValue
        <$> v .:? "description"
        <*> v .:? "is_deprecated"
        <*> v .: "value"

instance ToJSON InputObjectType where
    toJSON (InputObjectType descriptionInputObjectType fieldsInputObjectType nameInputObjectType) =
        object
        [ "description" .= descriptionInputObjectType
        , "fields" .= fieldsInputObjectType
        , "name" .= nameInputObjectType
        ]

instance FromJSON InputObjectType where
    parseJSON (Object v) = InputObjectType
        <$> v .:? "description"
        <*> v .: "fields"
        <*> v .: "name"

instance ToJSON InputObjectField where
    toJSON (InputObjectField descriptionInputObjectField nameInputObjectField inputObjectFieldTypeInputObjectField) =
        object
        [ "description" .= descriptionInputObjectField
        , "name" .= nameInputObjectField
        , "type" .= inputObjectFieldTypeInputObjectField
        ]

instance FromJSON InputObjectField where
    parseJSON (Object v) = InputObjectField
        <$> v .:? "description"
        <*> v .: "name"
        <*> v .: "type"

instance ToJSON ObjectType where
    toJSON (ObjectType descriptionObjectType fieldsObjectType nameObjectType relationshipsObjectType) =
        object
        [ "description" .= descriptionObjectType
        , "fields" .= fieldsObjectType
        , "name" .= nameObjectType
        , "relationships" .= relationshipsObjectType
        ]

instance FromJSON ObjectType where
    parseJSON (Object v) = ObjectType
        <$> v .:? "description"
        <*> v .: "fields"
        <*> v .: "name"
        <*> v .:? "relationships"

instance ToJSON CustomTypeObjectRelationship where
    toJSON (CustomTypeObjectRelationship fieldMappingCustomTypeObjectRelationship nameCustomTypeObjectRelationship remoteTableCustomTypeObjectRelationship customTypeObjectRelationshipTypeCustomTypeObjectRelationship) =
        object
        [ "field_mapping" .= fieldMappingCustomTypeObjectRelationship
        , "name" .= nameCustomTypeObjectRelationship
        , "remote_table" .= remoteTableCustomTypeObjectRelationship
        , "type" .= customTypeObjectRelationshipTypeCustomTypeObjectRelationship
        ]

instance FromJSON CustomTypeObjectRelationship where
    parseJSON (Object v) = CustomTypeObjectRelationship
        <$> v .: "field_mapping"
        <*> v .: "name"
        <*> v .: "remote_table"
        <*> v .: "type"

instance ToJSON CustomTypeObjectRelationshipType where
    toJSON TypeArrayCustomTypeObjectRelationshipType = "array"
    toJSON TypeObjectCustomTypeObjectRelationshipType = "object"

instance FromJSON CustomTypeObjectRelationshipType where
    parseJSON = withText "CustomTypeObjectRelationshipType" parseText
        where
            parseText "array" = return TypeArrayCustomTypeObjectRelationshipType
            parseText "object" = return TypeObjectCustomTypeObjectRelationshipType

instance ToJSON TableName where
    toJSON (QualifiedTableInTableName x) = toJSON x
    toJSON (StringInTableName x) = toJSON x

instance FromJSON TableName where
    parseJSON xs@(Object _) = (fmap QualifiedTableInTableName . parseJSON) xs
    parseJSON xs@(String _) = (fmap StringInTableName . parseJSON) xs

instance ToJSON QualifiedTable where
    toJSON (QualifiedTable nameQualifiedTable schemaQualifiedTable) =
        object
        [ "name" .= nameQualifiedTable
        , "schema" .= schemaQualifiedTable
        ]

instance FromJSON QualifiedTable where
    parseJSON (Object v) = QualifiedTable
        <$> v .: "name"
        <*> v .: "schema"

instance ToJSON ScalarType where
    toJSON (ScalarType descriptionScalarType nameScalarType) =
        object
        [ "description" .= descriptionScalarType
        , "name" .= nameScalarType
        ]

instance FromJSON ScalarType where
    parseJSON (Object v) = ScalarType
        <$> v .:? "description"
        <*> v .: "name"

instance ToJSON CustomFunction where
    toJSON (CustomFunction configurationCustomFunction functionCustomFunction) =
        object
        [ "configuration" .= configurationCustomFunction
        , "function" .= functionCustomFunction
        ]

instance FromJSON CustomFunction where
    parseJSON (Object v) = CustomFunction
        <$> v .:? "configuration"
        <*> v .: "function"

instance ToJSON FunctionConfiguration where
    toJSON (FunctionConfiguration sessionArgumentFunctionConfiguration) =
        object
        [ "session_argument" .= sessionArgumentFunctionConfiguration
        ]

instance FromJSON FunctionConfiguration where
    parseJSON (Object v) = FunctionConfiguration
        <$> v .:? "session_argument"

instance ToJSON FunctionName where
    toJSON (QualifiedFunctionInFunctionName x) = toJSON x
    toJSON (StringInFunctionName x) = toJSON x

instance FromJSON FunctionName where
    parseJSON xs@(Object _) = (fmap QualifiedFunctionInFunctionName . parseJSON) xs
    parseJSON xs@(String _) = (fmap StringInFunctionName . parseJSON) xs

instance ToJSON QualifiedFunction where
    toJSON (QualifiedFunction nameQualifiedFunction schemaQualifiedFunction) =
        object
        [ "name" .= nameQualifiedFunction
        , "schema" .= schemaQualifiedFunction
        ]

instance FromJSON QualifiedFunction where
    parseJSON (Object v) = QualifiedFunction
        <$> v .: "name"
        <*> v .: "schema"

instance ToJSON QueryCollectionEntry where
    toJSON (QueryCollectionEntry commentQueryCollectionEntry definitionQueryCollectionEntry nameQueryCollectionEntry) =
        object
        [ "comment" .= commentQueryCollectionEntry
        , "definition" .= definitionQueryCollectionEntry
        , "name" .= nameQueryCollectionEntry
        ]

instance FromJSON QueryCollectionEntry where
    parseJSON (Object v) = QueryCollectionEntry
        <$> v .:? "comment"
        <*> v .: "definition"
        <*> v .: "name"

instance ToJSON Definition where
    toJSON (Definition queriesDefinition) =
        object
        [ "queries" .= queriesDefinition
        ]

instance FromJSON Definition where
    parseJSON (Object v) = Definition
        <$> v .: "queries"

instance ToJSON QueryCollection where
    toJSON (QueryCollection nameQueryCollection queryQueryCollection) =
        object
        [ "name" .= nameQueryCollection
        , "query" .= queryQueryCollection
        ]

instance FromJSON QueryCollection where
    parseJSON (Object v) = QueryCollection
        <$> v .: "name"
        <*> v .: "query"

instance ToJSON RemoteSchema where
    toJSON (RemoteSchema commentRemoteSchema definitionRemoteSchema nameRemoteSchema) =
        object
        [ "comment" .= commentRemoteSchema
        , "definition" .= definitionRemoteSchema
        , "name" .= nameRemoteSchema
        ]

instance FromJSON RemoteSchema where
    parseJSON (Object v) = RemoteSchema
        <$> v .:? "comment"
        <*> v .: "definition"
        <*> v .: "name"

instance ToJSON RemoteSchemaDef where
    toJSON (RemoteSchemaDef forwardClientHeadersRemoteSchemaDef headersRemoteSchemaDef timeoutSecondsRemoteSchemaDef urlRemoteSchemaDef urlFromEnvRemoteSchemaDef) =
        object
        [ "forward_client_headers" .= forwardClientHeadersRemoteSchemaDef
        , "headers" .= headersRemoteSchemaDef
        , "timeout_seconds" .= timeoutSecondsRemoteSchemaDef
        , "url" .= urlRemoteSchemaDef
        , "url_from_env" .= urlFromEnvRemoteSchemaDef
        ]

instance FromJSON RemoteSchemaDef where
    parseJSON (Object v) = RemoteSchemaDef
        <$> v .:? "forward_client_headers"
        <*> v .:? "headers"
        <*> v .:? "timeout_seconds"
        <*> v .:? "url"
        <*> v .:? "url_from_env"

instance ToJSON TableEntry where
    toJSON (TableEntry arrayRelationshipsTableEntry computedFieldsTableEntry configurationTableEntry deletePermissionsTableEntry eventTriggersTableEntry insertPermissionsTableEntry isEnumTableEntry objectRelationshipsTableEntry remoteRelationshipsTableEntry selectPermissionsTableEntry tableTableEntry updatePermissionsTableEntry) =
        object
        [ "array_relationships" .= arrayRelationshipsTableEntry
        , "computed_fields" .= computedFieldsTableEntry
        , "configuration" .= configurationTableEntry
        , "delete_permissions" .= deletePermissionsTableEntry
        , "event_triggers" .= eventTriggersTableEntry
        , "insert_permissions" .= insertPermissionsTableEntry
        , "is_enum" .= isEnumTableEntry
        , "object_relationships" .= objectRelationshipsTableEntry
        , "remote_relationships" .= remoteRelationshipsTableEntry
        , "select_permissions" .= selectPermissionsTableEntry
        , "table" .= tableTableEntry
        , "update_permissions" .= updatePermissionsTableEntry
        ]

instance FromJSON TableEntry where
    parseJSON (Object v) = TableEntry
        <$> v .:? "array_relationships"
        <*> v .:? "computed_fields"
        <*> v .:? "configuration"
        <*> v .:? "delete_permissions"
        <*> v .:? "event_triggers"
        <*> v .:? "insert_permissions"
        <*> v .:? "is_enum"
        <*> v .:? "object_relationships"
        <*> v .:? "remote_relationships"
        <*> v .:? "select_permissions"
        <*> v .: "table"
        <*> v .:? "update_permissions"

instance ToJSON ArrayRelationship where
    toJSON (ArrayRelationship commentArrayRelationship nameArrayRelationship usingArrayRelationship) =
        object
        [ "comment" .= commentArrayRelationship
        , "name" .= nameArrayRelationship
        , "using" .= usingArrayRelationship
        ]

instance FromJSON ArrayRelationship where
    parseJSON (Object v) = ArrayRelationship
        <$> v .:? "comment"
        <*> v .: "name"
        <*> v .: "using"

instance ToJSON ArrRelUsing where
    toJSON (ArrRelUsing foreignKeyConstraintOnArrRelUsing manualConfigurationArrRelUsing) =
        object
        [ "foreign_key_constraint_on" .= foreignKeyConstraintOnArrRelUsing
        , "manual_configuration" .= manualConfigurationArrRelUsing
        ]

instance FromJSON ArrRelUsing where
    parseJSON (Object v) = ArrRelUsing
        <$> v .:? "foreign_key_constraint_on"
        <*> v .:? "manual_configuration"

instance ToJSON ArrRelUsingFKeyOn where
    toJSON (ArrRelUsingFKeyOn columnArrRelUsingFKeyOn tableArrRelUsingFKeyOn) =
        object
        [ "column" .= columnArrRelUsingFKeyOn
        , "table" .= tableArrRelUsingFKeyOn
        ]

instance FromJSON ArrRelUsingFKeyOn where
    parseJSON (Object v) = ArrRelUsingFKeyOn
        <$> v .: "column"
        <*> v .: "table"

instance ToJSON ArrRelUsingManualMapping where
    toJSON (ArrRelUsingManualMapping columnMappingArrRelUsingManualMapping remoteTableArrRelUsingManualMapping) =
        object
        [ "column_mapping" .= columnMappingArrRelUsingManualMapping
        , "remote_table" .= remoteTableArrRelUsingManualMapping
        ]

instance FromJSON ArrRelUsingManualMapping where
    parseJSON (Object v) = ArrRelUsingManualMapping
        <$> v .: "column_mapping"
        <*> v .: "remote_table"

instance ToJSON ComputedField where
    toJSON (ComputedField commentComputedField definitionComputedField nameComputedField) =
        object
        [ "comment" .= commentComputedField
        , "definition" .= definitionComputedField
        , "name" .= nameComputedField
        ]

instance FromJSON ComputedField where
    parseJSON (Object v) = ComputedField
        <$> v .:? "comment"
        <*> v .: "definition"
        <*> v .: "name"

instance ToJSON ComputedFieldDefinition where
    toJSON (ComputedFieldDefinition functionComputedFieldDefinition sessionArgumentComputedFieldDefinition tableArgumentComputedFieldDefinition) =
        object
        [ "function" .= functionComputedFieldDefinition
        , "session_argument" .= sessionArgumentComputedFieldDefinition
        , "table_argument" .= tableArgumentComputedFieldDefinition
        ]

instance FromJSON ComputedFieldDefinition where
    parseJSON (Object v) = ComputedFieldDefinition
        <$> v .: "function"
        <*> v .:? "session_argument"
        <*> v .:? "table_argument"

instance ToJSON TableConfig where
    toJSON (TableConfig customColumnNamesTableConfig customRootFieldsTableConfig) =
        object
        [ "custom_column_names" .= customColumnNamesTableConfig
        , "custom_root_fields" .= customRootFieldsTableConfig
        ]

instance FromJSON TableConfig where
    parseJSON (Object v) = TableConfig
        <$> v .:? "custom_column_names"
        <*> v .:? "custom_root_fields"

instance ToJSON CustomRootFields where
    toJSON (CustomRootFields deleteCustomRootFields deleteByPkCustomRootFields insertCustomRootFields insertOneCustomRootFields selectCustomRootFields selectAggregateCustomRootFields selectByPkCustomRootFields updateCustomRootFields updateByPkCustomRootFields) =
        object
        [ "delete" .= deleteCustomRootFields
        , "delete_by_pk" .= deleteByPkCustomRootFields
        , "insert" .= insertCustomRootFields
        , "insert_one" .= insertOneCustomRootFields
        , "select" .= selectCustomRootFields
        , "select_aggregate" .= selectAggregateCustomRootFields
        , "select_by_pk" .= selectByPkCustomRootFields
        , "update" .= updateCustomRootFields
        , "update_by_pk" .= updateByPkCustomRootFields
        ]

instance FromJSON CustomRootFields where
    parseJSON (Object v) = CustomRootFields
        <$> v .:? "delete"
        <*> v .:? "delete_by_pk"
        <*> v .:? "insert"
        <*> v .:? "insert_one"
        <*> v .:? "select"
        <*> v .:? "select_aggregate"
        <*> v .:? "select_by_pk"
        <*> v .:? "update"
        <*> v .:? "update_by_pk"

instance ToJSON DeletePermissionEntry where
    toJSON (DeletePermissionEntry commentDeletePermissionEntry permissionDeletePermissionEntry roleDeletePermissionEntry) =
        object
        [ "comment" .= commentDeletePermissionEntry
        , "permission" .= permissionDeletePermissionEntry
        , "role" .= roleDeletePermissionEntry
        ]

instance FromJSON DeletePermissionEntry where
    parseJSON (Object v) = DeletePermissionEntry
        <$> v .:? "comment"
        <*> v .: "permission"
        <*> v .: "role"

instance ToJSON DeletePermission where
    toJSON (DeletePermission filterDeletePermission) =
        object
        [ "filter" .= filterDeletePermission
        ]

instance FromJSON DeletePermission where
    parseJSON (Object v) = DeletePermission
        <$> v .:? "filter"

instance ToJSON Filter where
    toJSON (AnythingMapInFilter x) = toJSON x
    toJSON (DoubleInFilter x) = toJSON x
    toJSON (StringInFilter x) = toJSON x

instance FromJSON Filter where
    parseJSON xs@(Object _) = (fmap AnythingMapInFilter . parseJSON) xs
    parseJSON xs@(Number _) = (fmap DoubleInFilter . parseJSON) xs
    parseJSON xs@(String _) = (fmap StringInFilter . parseJSON) xs

instance ToJSON EventTrigger where
    toJSON (EventTrigger definitionEventTrigger headersEventTrigger nameEventTrigger retryConfEventTrigger webhookEventTrigger webhookFromEnvEventTrigger) =
        object
        [ "definition" .= definitionEventTrigger
        , "headers" .= headersEventTrigger
        , "name" .= nameEventTrigger
        , "retry_conf" .= retryConfEventTrigger
        , "webhook" .= webhookEventTrigger
        , "webhook_from_env" .= webhookFromEnvEventTrigger
        ]

instance FromJSON EventTrigger where
    parseJSON (Object v) = EventTrigger
        <$> v .: "definition"
        <*> v .:? "headers"
        <*> v .: "name"
        <*> v .: "retry_conf"
        <*> v .:? "webhook"
        <*> v .:? "webhook_from_env"

instance ToJSON EventTriggerDefinition where
    toJSON (EventTriggerDefinition deleteEventTriggerDefinition enableManualEventTriggerDefinition insertEventTriggerDefinition updateEventTriggerDefinition) =
        object
        [ "delete" .= deleteEventTriggerDefinition
        , "enable_manual" .= enableManualEventTriggerDefinition
        , "insert" .= insertEventTriggerDefinition
        , "update" .= updateEventTriggerDefinition
        ]

instance FromJSON EventTriggerDefinition where
    parseJSON (Object v) = EventTriggerDefinition
        <$> v .:? "delete"
        <*> v .: "enable_manual"
        <*> v .:? "insert"
        <*> v .:? "update"

instance ToJSON OperationSpec where
    toJSON (OperationSpec columnsOperationSpec payloadOperationSpec) =
        object
        [ "columns" .= columnsOperationSpec
        , "payload" .= payloadOperationSpec
        ]

instance FromJSON OperationSpec where
    parseJSON (Object v) = OperationSpec
        <$> v .: "columns"
        <*> v .:? "payload"

instance ToJSON EventTriggerColumns where
    toJSON (EnumInEventTriggerColumns x) = toJSON x
    toJSON (StringArrayInEventTriggerColumns x) = toJSON x

instance FromJSON EventTriggerColumns where
    parseJSON xs@(Object _) = (fmap EnumInEventTriggerColumns . parseJSON) xs
    parseJSON xs@(Array _) = (fmap StringArrayInEventTriggerColumns . parseJSON) xs

instance ToJSON Columns where
    toJSON EmptyColumns = "*"

instance FromJSON Columns where
    parseJSON = withText "Columns" parseText
        where
            parseText "*" = return EmptyColumns

instance ToJSON RetryConf where
    toJSON (RetryConf intervalSECRetryConf numRetriesRetryConf timeoutSECRetryConf) =
        object
        [ "interval_sec" .= intervalSECRetryConf
        , "num_retries" .= numRetriesRetryConf
        , "timeout_sec" .= timeoutSECRetryConf
        ]

instance FromJSON RetryConf where
    parseJSON (Object v) = RetryConf
        <$> v .:? "interval_sec"
        <*> v .:? "num_retries"
        <*> v .:? "timeout_sec"

instance ToJSON InsertPermissionEntry where
    toJSON (InsertPermissionEntry commentInsertPermissionEntry permissionInsertPermissionEntry roleInsertPermissionEntry) =
        object
        [ "comment" .= commentInsertPermissionEntry
        , "permission" .= permissionInsertPermissionEntry
        , "role" .= roleInsertPermissionEntry
        ]

instance FromJSON InsertPermissionEntry where
    parseJSON (Object v) = InsertPermissionEntry
        <$> v .:? "comment"
        <*> v .: "permission"
        <*> v .: "role"

instance ToJSON InsertPermission where
    toJSON (InsertPermission backendOnlyInsertPermission checkInsertPermission columnsInsertPermission setInsertPermission) =
        object
        [ "backend_only" .= backendOnlyInsertPermission
        , "check" .= checkInsertPermission
        , "columns" .= columnsInsertPermission
        , "set" .= setInsertPermission
        ]

instance FromJSON InsertPermission where
    parseJSON (Object v) = InsertPermission
        <$> v .:? "backend_only"
        <*> v .:? "check"
        <*> v .: "columns"
        <*> v .:? "set"

instance ToJSON ObjectRelationship where
    toJSON (ObjectRelationship commentObjectRelationship nameObjectRelationship usingObjectRelationship) =
        object
        [ "comment" .= commentObjectRelationship
        , "name" .= nameObjectRelationship
        , "using" .= usingObjectRelationship
        ]

instance FromJSON ObjectRelationship where
    parseJSON (Object v) = ObjectRelationship
        <$> v .:? "comment"
        <*> v .: "name"
        <*> v .: "using"

instance ToJSON ObjRelUsing where
    toJSON (ObjRelUsing foreignKeyConstraintOnObjRelUsing manualConfigurationObjRelUsing) =
        object
        [ "foreign_key_constraint_on" .= foreignKeyConstraintOnObjRelUsing
        , "manual_configuration" .= manualConfigurationObjRelUsing
        ]

instance FromJSON ObjRelUsing where
    parseJSON (Object v) = ObjRelUsing
        <$> v .:? "foreign_key_constraint_on"
        <*> v .:? "manual_configuration"

instance ToJSON ObjRelUsingManualMapping where
    toJSON (ObjRelUsingManualMapping columnMappingObjRelUsingManualMapping remoteTableObjRelUsingManualMapping) =
        object
        [ "column_mapping" .= columnMappingObjRelUsingManualMapping
        , "remote_table" .= remoteTableObjRelUsingManualMapping
        ]

instance FromJSON ObjRelUsingManualMapping where
    parseJSON (Object v) = ObjRelUsingManualMapping
        <$> v .: "column_mapping"
        <*> v .: "remote_table"

instance ToJSON RemoteRelationship where
    toJSON (RemoteRelationship definitionRemoteRelationship nameRemoteRelationship) =
        object
        [ "definition" .= definitionRemoteRelationship
        , "name" .= nameRemoteRelationship
        ]

instance FromJSON RemoteRelationship where
    parseJSON (Object v) = RemoteRelationship
        <$> v .: "definition"
        <*> v .: "name"

instance ToJSON RemoteRelationshipDef where
    toJSON (RemoteRelationshipDef hasuraFieldsRemoteRelationshipDef remoteFieldRemoteRelationshipDef remoteSchemaRemoteRelationshipDef) =
        object
        [ "hasura_fields" .= hasuraFieldsRemoteRelationshipDef
        , "remote_field" .= remoteFieldRemoteRelationshipDef
        , "remote_schema" .= remoteSchemaRemoteRelationshipDef
        ]

instance FromJSON RemoteRelationshipDef where
    parseJSON (Object v) = RemoteRelationshipDef
        <$> v .: "hasura_fields"
        <*> v .: "remote_field"
        <*> v .: "remote_schema"

instance ToJSON RemoteFieldValue where
    toJSON (RemoteFieldValue argumentsRemoteFieldValue fieldRemoteFieldValue) =
        object
        [ "arguments" .= argumentsRemoteFieldValue
        , "field" .= fieldRemoteFieldValue
        ]

instance FromJSON RemoteFieldValue where
    parseJSON (Object v) = RemoteFieldValue
        <$> v .: "arguments"
        <*> v .:? "field"

instance ToJSON SelectPermissionEntry where
    toJSON (SelectPermissionEntry commentSelectPermissionEntry permissionSelectPermissionEntry roleSelectPermissionEntry) =
        object
        [ "comment" .= commentSelectPermissionEntry
        , "permission" .= permissionSelectPermissionEntry
        , "role" .= roleSelectPermissionEntry
        ]

instance FromJSON SelectPermissionEntry where
    parseJSON (Object v) = SelectPermissionEntry
        <$> v .:? "comment"
        <*> v .: "permission"
        <*> v .: "role"

instance ToJSON SelectPermission where
    toJSON (SelectPermission allowAggregationsSelectPermission columnsSelectPermission computedFieldsSelectPermission filterSelectPermission limitSelectPermission) =
        object
        [ "allow_aggregations" .= allowAggregationsSelectPermission
        , "columns" .= columnsSelectPermission
        , "computed_fields" .= computedFieldsSelectPermission
        , "filter" .= filterSelectPermission
        , "limit" .= limitSelectPermission
        ]

instance FromJSON SelectPermission where
    parseJSON (Object v) = SelectPermission
        <$> v .:? "allow_aggregations"
        <*> v .: "columns"
        <*> v .:? "computed_fields"
        <*> v .:? "filter"
        <*> v .:? "limit"

instance ToJSON UpdatePermissionEntry where
    toJSON (UpdatePermissionEntry commentUpdatePermissionEntry permissionUpdatePermissionEntry roleUpdatePermissionEntry) =
        object
        [ "comment" .= commentUpdatePermissionEntry
        , "permission" .= permissionUpdatePermissionEntry
        , "role" .= roleUpdatePermissionEntry
        ]

instance FromJSON UpdatePermissionEntry where
    parseJSON (Object v) = UpdatePermissionEntry
        <$> v .:? "comment"
        <*> v .: "permission"
        <*> v .: "role"

instance ToJSON UpdatePermission where
    toJSON (UpdatePermission checkUpdatePermission columnsUpdatePermission filterUpdatePermission setUpdatePermission) =
        object
        [ "check" .= checkUpdatePermission
        , "columns" .= columnsUpdatePermission
        , "filter" .= filterUpdatePermission
        , "set" .= setUpdatePermission
        ]

instance FromJSON UpdatePermission where
    parseJSON (Object v) = UpdatePermission
        <$> v .:? "check"
        <*> v .: "columns"
        <*> v .:? "filter"
        <*> v .:? "set"
