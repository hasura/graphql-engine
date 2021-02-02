// This file was generated from JSON Schema using quicktype, do not modify it directly.
// To parse and unparse this JSON data, add this code to your project and do:
//
//    pGColumn, err := UnmarshalPGColumn(bytes)
//    bytes, err = pGColumn.Marshal()
//
//    computedFieldName, err := UnmarshalComputedFieldName(bytes)
//    bytes, err = computedFieldName.Marshal()
//
//    roleName, err := UnmarshalRoleName(bytes)
//    bytes, err = roleName.Marshal()
//
//    triggerName, err := UnmarshalTriggerName(bytes)
//    bytes, err = triggerName.Marshal()
//
//    remoteRelationshipName, err := UnmarshalRemoteRelationshipName(bytes)
//    bytes, err = remoteRelationshipName.Marshal()
//
//    remoteSchemaName, err := UnmarshalRemoteSchemaName(bytes)
//    bytes, err = remoteSchemaName.Marshal()
//
//    collectionName, err := UnmarshalCollectionName(bytes)
//    bytes, err = collectionName.Marshal()
//
//    graphQLName, err := UnmarshalGraphQLName(bytes)
//    bytes, err = graphQLName.Marshal()
//
//    graphQLType, err := UnmarshalGraphQLType(bytes)
//    bytes, err = graphQLType.Marshal()
//
//    relationshipName, err := UnmarshalRelationshipName(bytes)
//    bytes, err = relationshipName.Marshal()
//
//    actionName, err := UnmarshalActionName(bytes)
//    bytes, err = actionName.Marshal()
//
//    webhookURL, err := UnmarshalWebhookURL(bytes)
//    bytes, err = webhookURL.Marshal()
//
//    tableName, err := UnmarshalTableName(bytes)
//    bytes, err = tableName.Marshal()
//
//    qualifiedTable, err := UnmarshalQualifiedTable(bytes)
//    bytes, err = qualifiedTable.Marshal()
//
//    tableConfig, err := UnmarshalTableConfig(bytes)
//    bytes, err = tableConfig.Marshal()
//
//    tableEntry, err := UnmarshalTableEntry(bytes)
//    bytes, err = tableEntry.Marshal()
//
//    customRootFields, err := UnmarshalCustomRootFields(bytes)
//    bytes, err = customRootFields.Marshal()
//
//    customColumnNames, err := UnmarshalCustomColumnNames(bytes)
//    bytes, err = customColumnNames.Marshal()
//
//    functionName, err := UnmarshalFunctionName(bytes)
//    bytes, err = functionName.Marshal()
//
//    qualifiedFunction, err := UnmarshalQualifiedFunction(bytes)
//    bytes, err = qualifiedFunction.Marshal()
//
//    customFunction, err := UnmarshalCustomFunction(bytes)
//    bytes, err = customFunction.Marshal()
//
//    functionConfiguration, err := UnmarshalFunctionConfiguration(bytes)
//    bytes, err = functionConfiguration.Marshal()
//
//    objectRelationship, err := UnmarshalObjectRelationship(bytes)
//    bytes, err = objectRelationship.Marshal()
//
//    objRelUsing, err := UnmarshalObjRelUsing(bytes)
//    bytes, err = objRelUsing.Marshal()
//
//    objRelUsingManualMapping, err := UnmarshalObjRelUsingManualMapping(bytes)
//    bytes, err = objRelUsingManualMapping.Marshal()
//
//    arrayRelationship, err := UnmarshalArrayRelationship(bytes)
//    bytes, err = arrayRelationship.Marshal()
//
//    arrRelUsing, err := UnmarshalArrRelUsing(bytes)
//    bytes, err = arrRelUsing.Marshal()
//
//    arrRelUsingFKeyOn, err := UnmarshalArrRelUsingFKeyOn(bytes)
//    bytes, err = arrRelUsingFKeyOn.Marshal()
//
//    arrRelUsingManualMapping, err := UnmarshalArrRelUsingManualMapping(bytes)
//    bytes, err = arrRelUsingManualMapping.Marshal()
//
//    columnPresetsExpression, err := UnmarshalColumnPresetsExpression(bytes)
//    bytes, err = columnPresetsExpression.Marshal()
//
//    insertPermissionEntry, err := UnmarshalInsertPermissionEntry(bytes)
//    bytes, err = insertPermissionEntry.Marshal()
//
//    insertPermission, err := UnmarshalInsertPermission(bytes)
//    bytes, err = insertPermission.Marshal()
//
//    selectPermissionEntry, err := UnmarshalSelectPermissionEntry(bytes)
//    bytes, err = selectPermissionEntry.Marshal()
//
//    selectPermission, err := UnmarshalSelectPermission(bytes)
//    bytes, err = selectPermission.Marshal()
//
//    updatePermissionEntry, err := UnmarshalUpdatePermissionEntry(bytes)
//    bytes, err = updatePermissionEntry.Marshal()
//
//    updatePermission, err := UnmarshalUpdatePermission(bytes)
//    bytes, err = updatePermission.Marshal()
//
//    deletePermissionEntry, err := UnmarshalDeletePermissionEntry(bytes)
//    bytes, err = deletePermissionEntry.Marshal()
//
//    deletePermission, err := UnmarshalDeletePermission(bytes)
//    bytes, err = deletePermission.Marshal()
//
//    computedField, err := UnmarshalComputedField(bytes)
//    bytes, err = computedField.Marshal()
//
//    computedFieldDefinition, err := UnmarshalComputedFieldDefinition(bytes)
//    bytes, err = computedFieldDefinition.Marshal()
//
//    eventTrigger, err := UnmarshalEventTrigger(bytes)
//    bytes, err = eventTrigger.Marshal()
//
//    eventTriggerDefinition, err := UnmarshalEventTriggerDefinition(bytes)
//    bytes, err = eventTriggerDefinition.Marshal()
//
//    eventTriggerColumns, err := UnmarshalEventTriggerColumns(bytes)
//    bytes, err = eventTriggerColumns.Marshal()
//
//    operationSpec, err := UnmarshalOperationSpec(bytes)
//    bytes, err = operationSpec.Marshal()
//
//    headerFromValue, err := UnmarshalHeaderFromValue(bytes)
//    bytes, err = headerFromValue.Marshal()
//
//    headerFromEnv, err := UnmarshalHeaderFromEnv(bytes)
//    bytes, err = headerFromEnv.Marshal()
//
//    retryConf, err := UnmarshalRetryConf(bytes)
//    bytes, err = retryConf.Marshal()
//
//    cronTrigger, err := UnmarshalCronTrigger(bytes)
//    bytes, err = cronTrigger.Marshal()
//
//    retryConfST, err := UnmarshalRetryConfST(bytes)
//    bytes, err = retryConfST.Marshal()
//
//    remoteSchema, err := UnmarshalRemoteSchema(bytes)
//    bytes, err = remoteSchema.Marshal()
//
//    remoteSchemaDef, err := UnmarshalRemoteSchemaDef(bytes)
//    bytes, err = remoteSchemaDef.Marshal()
//
//    remoteRelationship, err := UnmarshalRemoteRelationship(bytes)
//    bytes, err = remoteRelationship.Marshal()
//
//    remoteRelationshipDef, err := UnmarshalRemoteRelationshipDef(bytes)
//    bytes, err = remoteRelationshipDef.Marshal()
//
//    remoteField, err := UnmarshalRemoteField(bytes)
//    bytes, err = remoteField.Marshal()
//
//    inputArguments, err := UnmarshalInputArguments(bytes)
//    bytes, err = inputArguments.Marshal()
//
//    queryCollectionEntry, err := UnmarshalQueryCollectionEntry(bytes)
//    bytes, err = queryCollectionEntry.Marshal()
//
//    queryCollection, err := UnmarshalQueryCollection(bytes)
//    bytes, err = queryCollection.Marshal()
//
//    allowList, err := UnmarshalAllowList(bytes)
//    bytes, err = allowList.Marshal()
//
//    customTypes, err := UnmarshalCustomTypes(bytes)
//    bytes, err = customTypes.Marshal()
//
//    inputObjectType, err := UnmarshalInputObjectType(bytes)
//    bytes, err = inputObjectType.Marshal()
//
//    inputObjectField, err := UnmarshalInputObjectField(bytes)
//    bytes, err = inputObjectField.Marshal()
//
//    objectType, err := UnmarshalObjectType(bytes)
//    bytes, err = objectType.Marshal()
//
//    objectField, err := UnmarshalObjectField(bytes)
//    bytes, err = objectField.Marshal()
//
//    customTypeObjectRelationship, err := UnmarshalCustomTypeObjectRelationship(bytes)
//    bytes, err = customTypeObjectRelationship.Marshal()
//
//    scalarType, err := UnmarshalScalarType(bytes)
//    bytes, err = scalarType.Marshal()
//
//    enumType, err := UnmarshalEnumType(bytes)
//    bytes, err = enumType.Marshal()
//
//    enumValue, err := UnmarshalEnumValue(bytes)
//    bytes, err = enumValue.Marshal()
//
//    action, err := UnmarshalAction(bytes)
//    bytes, err = action.Marshal()
//
//    actionDefinition, err := UnmarshalActionDefinition(bytes)
//    bytes, err = actionDefinition.Marshal()
//
//    inputArgument, err := UnmarshalInputArgument(bytes)
//    bytes, err = inputArgument.Marshal()
//
//    hasuraMetadataV2, err := UnmarshalHasuraMetadataV2(bytes)
//    bytes, err = hasuraMetadataV2.Marshal()

package hasura_metadata

import "bytes"
import "errors"
import "encoding/json"

type PGColumn string

func UnmarshalPGColumn(data []byte) (PGColumn, error) {
	var r PGColumn
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *PGColumn) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type ComputedFieldName string

func UnmarshalComputedFieldName(data []byte) (ComputedFieldName, error) {
	var r ComputedFieldName
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ComputedFieldName) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type RoleName string

func UnmarshalRoleName(data []byte) (RoleName, error) {
	var r RoleName
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *RoleName) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type TriggerName string

func UnmarshalTriggerName(data []byte) (TriggerName, error) {
	var r TriggerName
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *TriggerName) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type RemoteRelationshipName string

func UnmarshalRemoteRelationshipName(data []byte) (RemoteRelationshipName, error) {
	var r RemoteRelationshipName
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *RemoteRelationshipName) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type RemoteSchemaName string

func UnmarshalRemoteSchemaName(data []byte) (RemoteSchemaName, error) {
	var r RemoteSchemaName
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *RemoteSchemaName) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type CollectionName string

func UnmarshalCollectionName(data []byte) (CollectionName, error) {
	var r CollectionName
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *CollectionName) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type GraphQLName string

func UnmarshalGraphQLName(data []byte) (GraphQLName, error) {
	var r GraphQLName
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *GraphQLName) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type GraphQLType string

func UnmarshalGraphQLType(data []byte) (GraphQLType, error) {
	var r GraphQLType
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *GraphQLType) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type RelationshipName string

func UnmarshalRelationshipName(data []byte) (RelationshipName, error) {
	var r RelationshipName
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *RelationshipName) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type ActionName string

func UnmarshalActionName(data []byte) (ActionName, error) {
	var r ActionName
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ActionName) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type WebhookURL string

func UnmarshalWebhookURL(data []byte) (WebhookURL, error) {
	var r WebhookURL
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *WebhookURL) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalTableName(data []byte) (TableName, error) {
	var r TableName
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *TableName) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalQualifiedTable(data []byte) (QualifiedTable, error) {
	var r QualifiedTable
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *QualifiedTable) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalTableConfig(data []byte) (TableConfig, error) {
	var r TableConfig
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *TableConfig) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalTableEntry(data []byte) (TableEntry, error) {
	var r TableEntry
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *TableEntry) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalCustomRootFields(data []byte) (CustomRootFields, error) {
	var r CustomRootFields
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *CustomRootFields) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type CustomColumnNames map[string]string

func UnmarshalCustomColumnNames(data []byte) (CustomColumnNames, error) {
	var r CustomColumnNames
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *CustomColumnNames) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalFunctionName(data []byte) (FunctionName, error) {
	var r FunctionName
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *FunctionName) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalQualifiedFunction(data []byte) (QualifiedFunction, error) {
	var r QualifiedFunction
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *QualifiedFunction) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalCustomFunction(data []byte) (CustomFunction, error) {
	var r CustomFunction
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *CustomFunction) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalFunctionConfiguration(data []byte) (FunctionConfiguration, error) {
	var r FunctionConfiguration
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *FunctionConfiguration) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalObjectRelationship(data []byte) (ObjectRelationship, error) {
	var r ObjectRelationship
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ObjectRelationship) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalObjRelUsing(data []byte) (ObjRelUsing, error) {
	var r ObjRelUsing
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ObjRelUsing) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalObjRelUsingManualMapping(data []byte) (ObjRelUsingManualMapping, error) {
	var r ObjRelUsingManualMapping
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ObjRelUsingManualMapping) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalArrayRelationship(data []byte) (ArrayRelationship, error) {
	var r ArrayRelationship
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ArrayRelationship) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalArrRelUsing(data []byte) (ArrRelUsing, error) {
	var r ArrRelUsing
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ArrRelUsing) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalArrRelUsingFKeyOn(data []byte) (ArrRelUsingFKeyOn, error) {
	var r ArrRelUsingFKeyOn
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ArrRelUsingFKeyOn) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalArrRelUsingManualMapping(data []byte) (ArrRelUsingManualMapping, error) {
	var r ArrRelUsingManualMapping
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ArrRelUsingManualMapping) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type ColumnPresetsExpression map[string]string

func UnmarshalColumnPresetsExpression(data []byte) (ColumnPresetsExpression, error) {
	var r ColumnPresetsExpression
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ColumnPresetsExpression) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalInsertPermissionEntry(data []byte) (InsertPermissionEntry, error) {
	var r InsertPermissionEntry
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *InsertPermissionEntry) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalInsertPermission(data []byte) (InsertPermission, error) {
	var r InsertPermission
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *InsertPermission) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalSelectPermissionEntry(data []byte) (SelectPermissionEntry, error) {
	var r SelectPermissionEntry
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *SelectPermissionEntry) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalSelectPermission(data []byte) (SelectPermission, error) {
	var r SelectPermission
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *SelectPermission) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalUpdatePermissionEntry(data []byte) (UpdatePermissionEntry, error) {
	var r UpdatePermissionEntry
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *UpdatePermissionEntry) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalUpdatePermission(data []byte) (UpdatePermission, error) {
	var r UpdatePermission
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *UpdatePermission) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalDeletePermissionEntry(data []byte) (DeletePermissionEntry, error) {
	var r DeletePermissionEntry
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *DeletePermissionEntry) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalDeletePermission(data []byte) (DeletePermission, error) {
	var r DeletePermission
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *DeletePermission) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalComputedField(data []byte) (ComputedField, error) {
	var r ComputedField
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ComputedField) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalComputedFieldDefinition(data []byte) (ComputedFieldDefinition, error) {
	var r ComputedFieldDefinition
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ComputedFieldDefinition) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalEventTrigger(data []byte) (EventTrigger, error) {
	var r EventTrigger
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *EventTrigger) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalEventTriggerDefinition(data []byte) (EventTriggerDefinition, error) {
	var r EventTriggerDefinition
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *EventTriggerDefinition) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalEventTriggerColumns(data []byte) (EventTriggerColumns, error) {
	var r EventTriggerColumns
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *EventTriggerColumns) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalOperationSpec(data []byte) (OperationSpec, error) {
	var r OperationSpec
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *OperationSpec) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalHeaderFromValue(data []byte) (HeaderFromValue, error) {
	var r HeaderFromValue
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *HeaderFromValue) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalHeaderFromEnv(data []byte) (HeaderFromEnv, error) {
	var r HeaderFromEnv
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *HeaderFromEnv) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalRetryConf(data []byte) (RetryConf, error) {
	var r RetryConf
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *RetryConf) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalCronTrigger(data []byte) (CronTrigger, error) {
	var r CronTrigger
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *CronTrigger) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalRetryConfST(data []byte) (RetryConfST, error) {
	var r RetryConfST
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *RetryConfST) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalRemoteSchema(data []byte) (RemoteSchema, error) {
	var r RemoteSchema
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *RemoteSchema) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalRemoteSchemaDef(data []byte) (RemoteSchemaDef, error) {
	var r RemoteSchemaDef
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *RemoteSchemaDef) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalRemoteRelationship(data []byte) (RemoteRelationship, error) {
	var r RemoteRelationship
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *RemoteRelationship) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalRemoteRelationshipDef(data []byte) (RemoteRelationshipDef, error) {
	var r RemoteRelationshipDef
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *RemoteRelationshipDef) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type RemoteField map[string]RemoteFieldValue

func UnmarshalRemoteField(data []byte) (RemoteField, error) {
	var r RemoteField
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *RemoteField) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

type InputArguments map[string]string

func UnmarshalInputArguments(data []byte) (InputArguments, error) {
	var r InputArguments
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *InputArguments) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalQueryCollectionEntry(data []byte) (QueryCollectionEntry, error) {
	var r QueryCollectionEntry
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *QueryCollectionEntry) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalQueryCollection(data []byte) (QueryCollection, error) {
	var r QueryCollection
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *QueryCollection) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalAllowList(data []byte) (AllowList, error) {
	var r AllowList
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *AllowList) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalCustomTypes(data []byte) (CustomTypes, error) {
	var r CustomTypes
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *CustomTypes) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalInputObjectType(data []byte) (InputObjectType, error) {
	var r InputObjectType
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *InputObjectType) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalInputObjectField(data []byte) (InputObjectField, error) {
	var r InputObjectField
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *InputObjectField) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalObjectType(data []byte) (ObjectType, error) {
	var r ObjectType
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ObjectType) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalObjectField(data []byte) (ObjectField, error) {
	var r ObjectField
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ObjectField) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalCustomTypeObjectRelationship(data []byte) (CustomTypeObjectRelationship, error) {
	var r CustomTypeObjectRelationship
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *CustomTypeObjectRelationship) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalScalarType(data []byte) (ScalarType, error) {
	var r ScalarType
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ScalarType) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalEnumType(data []byte) (EnumType, error) {
	var r EnumType
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *EnumType) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalEnumValue(data []byte) (EnumValue, error) {
	var r EnumValue
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *EnumValue) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalAction(data []byte) (Action, error) {
	var r Action
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *Action) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalActionDefinition(data []byte) (ActionDefinition, error) {
	var r ActionDefinition
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *ActionDefinition) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalInputArgument(data []byte) (InputArgument, error) {
	var r InputArgument
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *InputArgument) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

func UnmarshalHasuraMetadataV2(data []byte) (HasuraMetadataV2, error) {
	var r HasuraMetadataV2
	err := json.Unmarshal(data, &r)
	return r, err
}

func (r *HasuraMetadataV2) Marshal() ([]byte, error) {
	return json.Marshal(r)
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromvalue
type HeaderFromValue struct {
	Name  string `json:"name"` // Name of the header
	Value string `json:"value"`// Value of the header
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromenv
type HeaderFromEnv struct {
	Name         string `json:"name"`          // Name of the header
	ValueFromEnv string `json:"value_from_env"`// Name of the environment variable which holds the value of the header
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#objectfield
type ObjectField struct {
	Description *string `json:"description,omitempty"`// Description of the Input object type
	Name        string  `json:"name"`                 // Name of the Input object type
	Type        string  `json:"type"`                 // GraphQL type of the Input object type
}

// Type used in exported 'metadata.json' and replace metadata endpoint
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/manage-metadata.html#replace-metadata
type HasuraMetadataV2 struct {
	Actions          []Action               `json:"actions"`               
	Allowlist        []AllowList            `json:"allowlist"`             
	CronTriggers     []CronTrigger          `json:"cron_triggers"`         
	CustomTypes      *CustomTypes           `json:"custom_types,omitempty"`
	Functions        []CustomFunction       `json:"functions"`             
	QueryCollections []QueryCollectionEntry `json:"query_collections"`     
	RemoteSchemas    []RemoteSchema         `json:"remote_schemas"`        
	Tables           []TableEntry           `json:"tables"`                
	Version          float64                `json:"version"`               
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/actions.html#args-syntax
type Action struct {
	Comment     *string          `json:"comment,omitempty"`    // Comment
	Definition  ActionDefinition `json:"definition"`           // Definition of the action
	Name        string           `json:"name"`                 // Name of the action
	Permissions *Permissions     `json:"permissions,omitempty"`// Permissions of the action
}

// Definition of the action
//
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/actions.html#actiondefinition
type ActionDefinition struct {
	Arguments            []InputArgument       `json:"arguments"`                       
	ForwardClientHeaders *bool                 `json:"forward_client_headers,omitempty"`
	Handler              string                `json:"handler"`                         // A String value which supports templating environment variables enclosed in {{ and }}.; Template example: https://{{ACTION_API_DOMAIN}}/create-user
	Headers              []Header              `json:"headers"`                         
	Kind                 *string               `json:"kind,omitempty"`                  
	OutputType           *string               `json:"output_type,omitempty"`           
	Type                 *ActionDefinitionType `json:"type,omitempty"`                  
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/actions.html#inputargument
type InputArgument struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromvalue
//
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromenv
type Header struct {
	Name         string  `json:"name"`                    // Name of the header
	Value        *string `json:"value,omitempty"`         // Value of the header
	ValueFromEnv *string `json:"value_from_env,omitempty"`// Name of the environment variable which holds the value of the header
}

// Permissions of the action
type Permissions struct {
	Role string `json:"role"`
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/query-collections.html#add-collection-to-allowlist-syntax
type AllowList struct {
	Collection string `json:"collection"`// Name of a query collection to be added to the allow-list
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/scheduled-triggers.html#create-cron-trigger
type CronTrigger struct {
	Comment           *string                `json:"comment,omitempty"`   // Custom comment.
	Headers           []Header               `json:"headers"`             // List of headers to be sent with the webhook
	IncludeInMetadata bool                   `json:"include_in_metadata"` // Flag to indicate whether a trigger should be included in the metadata. When a cron; trigger is included in the metadata, the user will be able to export it when the metadata; of the graphql-engine is exported.
	Name              string                 `json:"name"`                // Name of the cron trigger
	Payload           map[string]interface{} `json:"payload,omitempty"`   // Any JSON payload which will be sent when the webhook is invoked.
	RetryConf         *RetryConfST           `json:"retry_conf,omitempty"`// Retry configuration if scheduled invocation delivery fails
	Schedule          string                 `json:"schedule"`            // Cron expression at which the trigger should be invoked.
	Webhook           string                 `json:"webhook"`             // URL of the webhook
}

// Retry configuration if scheduled invocation delivery fails
//
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/scheduled-triggers.html#retryconfst
type RetryConfST struct {
	NumRetries           *int64 `json:"num_retries,omitempty"`           // Number of times to retry delivery.; Default: 0
	RetryIntervalSeconds *int64 `json:"retry_interval_seconds,omitempty"`// Number of seconds to wait between each retry.; Default: 10
	TimeoutSeconds       *int64 `json:"timeout_seconds,omitempty"`       // Number of seconds to wait for response before timing out.; Default: 60
	ToleranceSeconds     *int64 `json:"tolerance_seconds,omitempty"`     // Number of seconds between scheduled time and actual delivery time that is acceptable. If; the time difference is more than this, then the event is dropped.; Default: 21600 (6 hours)
}

type CustomTypes struct {
	Enums        []EnumType        `json:"enums"`        
	InputObjects []InputObjectType `json:"input_objects"`
	Objects      []ObjectType      `json:"objects"`      
	Scalars      []ScalarType      `json:"scalars"`      
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#enumtype
type EnumType struct {
	Description *string     `json:"description,omitempty"`// Description of the Enum type
	Name        string      `json:"name"`                 // Name of the Enum type
	Values      []EnumValue `json:"values"`               // Values of the Enum type
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#enumvalue
type EnumValue struct {
	Description  *string `json:"description,omitempty"`  // Description of the Enum value
	IsDeprecated *bool   `json:"is_deprecated,omitempty"`// If set to true, the enum value is marked as deprecated
	Value        string  `json:"value"`                  // Value of the Enum type
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#inputobjecttype
type InputObjectType struct {
	Description *string            `json:"description,omitempty"`// Description of the Input object type
	Fields      []InputObjectField `json:"fields"`               // Fields of the Input object type
	Name        string             `json:"name"`                 // Name of the Input object type
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#inputobjectfield
type InputObjectField struct {
	Description *string `json:"description,omitempty"`// Description of the Input object type
	Name        string  `json:"name"`                 // Name of the Input object type
	Type        string  `json:"type"`                 // GraphQL type of the Input object type
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#objecttype
type ObjectType struct {
	Description   *string                        `json:"description,omitempty"`// Description of the Input object type
	Fields        []InputObjectField             `json:"fields"`               // Fields of the Input object type
	Name          string                         `json:"name"`                 // Name of the Input object type
	Relationships []CustomTypeObjectRelationship `json:"relationships"`        // Relationships of the Object type to tables
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#objectrelationship
type CustomTypeObjectRelationship struct {
	FieldMapping map[string]string                `json:"field_mapping"`// Mapping of fields of object type to columns of remote table
	Name         string                           `json:"name"`         // Name of the relationship, shouldn’t conflict with existing field names
	RemoteTable  *TableName                       `json:"remote_table"` // The table to which relationship is defined
	Type         CustomTypeObjectRelationshipType `json:"type"`         // Type of the relationship
}

type QualifiedTable struct {
	Name   string `json:"name"`  
	Schema string `json:"schema"`
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#scalartype
type ScalarType struct {
	Description *string `json:"description,omitempty"`// Description of the Scalar type
	Name        string  `json:"name"`                 // Name of the Scalar type
}

// A custom SQL function to add to the GraphQL schema with configuration.
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-functions.html#args-syntax
type CustomFunction struct {
	Configuration *FunctionConfiguration `json:"configuration,omitempty"`// Configuration for the SQL function
	Function      *FunctionName          `json:"function"`               // Name of the SQL function
}

// Configuration for the SQL function
//
// Configuration for a CustomFunction
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-functions.html#function-configuration
type FunctionConfiguration struct {
	SessionArgument *string `json:"session_argument,omitempty"`// Function argument which accepts session info JSON; Currently, only functions which satisfy the following constraints can be exposed over the; GraphQL API (terminology from Postgres docs):; - Function behaviour: ONLY `STABLE` or `IMMUTABLE`; - Return type: MUST be `SETOF <table-name>`; - Argument modes: ONLY `IN`
}

type QualifiedFunction struct {
	Name   string `json:"name"`  
	Schema string `json:"schema"`
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/query-collections.html#args-syntax
type QueryCollectionEntry struct {
	Comment    *string    `json:"comment,omitempty"`// Comment
	Definition Definition `json:"definition"`       // List of queries
	Name       string     `json:"name"`             // Name of the query collection
}

// List of queries
type Definition struct {
	Queries []QueryCollection `json:"queries"`
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#collectionquery
type QueryCollection struct {
	Name  string `json:"name"` 
	Query string `json:"query"`
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/remote-schemas.html#add-remote-schema
type RemoteSchema struct {
	Comment    *string         `json:"comment,omitempty"`// Comment
	Definition RemoteSchemaDef `json:"definition"`       // Name of the remote schema
	Name       string          `json:"name"`             // Name of the remote schema
}

// Name of the remote schema
//
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#remoteschemadef
type RemoteSchemaDef struct {
	ForwardClientHeaders *bool    `json:"forward_client_headers,omitempty"`
	Headers              []Header `json:"headers"`                         
	TimeoutSeconds       *float64 `json:"timeout_seconds,omitempty"`       
	URL                  *string  `json:"url,omitempty"`                   
	URLFromEnv           *string  `json:"url_from_env,omitempty"`          
}

// Representation of a table in metadata, 'tables.yaml' and 'metadata.json'
type TableEntry struct {
	ArrayRelationships  []ArrayRelationship     `json:"array_relationships"`    
	ComputedFields      []ComputedField         `json:"computed_fields"`        
	Configuration       *TableConfig            `json:"configuration,omitempty"`// Configuration for the table/view; ; https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/table-view.html#table-config
	DeletePermissions   []DeletePermissionEntry `json:"delete_permissions"`     
	EventTriggers       []EventTrigger          `json:"event_triggers"`         
	InsertPermissions   []InsertPermissionEntry `json:"insert_permissions"`     
	IsEnum              *bool                   `json:"is_enum,omitempty"`      
	ObjectRelationships []ObjectRelationship    `json:"object_relationships"`   
	RemoteRelationships []RemoteRelationship    `json:"remote_relationships"`   
	SelectPermissions   []SelectPermissionEntry `json:"select_permissions"`     
	Table               QualifiedTable          `json:"table"`                  
	UpdatePermissions   []UpdatePermissionEntry `json:"update_permissions"`     
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#create-array-relationship-syntax
type ArrayRelationship struct {
	Comment *string     `json:"comment,omitempty"`// Comment
	Name    string      `json:"name"`             // Name of the new relationship
	Using   ArrRelUsing `json:"using"`            // Use one of the available ways to define an array relationship
}

// Use one of the available ways to define an array relationship
//
// Use one of the available ways to define an object relationship
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#arrrelusing
type ArrRelUsing struct {
	ForeignKeyConstraintOn *ArrRelUsingFKeyOn        `json:"foreign_key_constraint_on,omitempty"`// The column with foreign key constraint
	ManualConfiguration    *ArrRelUsingManualMapping `json:"manual_configuration,omitempty"`     // Manual mapping of table and columns
}

// The column with foreign key constraint
//
// The column with foreign key constraint
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#arrrelusingfkeyon
type ArrRelUsingFKeyOn struct {
	Column string     `json:"column"`
	Table  *TableName `json:"table"` 
}

// Manual mapping of table and columns
//
// Manual mapping of table and columns
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#arrrelusingmanualmapping
type ArrRelUsingManualMapping struct {
	ColumnMapping map[string]string `json:"column_mapping"`// Mapping of columns from current table to remote table
	RemoteTable   *TableName        `json:"remote_table"`  // The table to which the relationship has to be established
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/computed-field.html#args-syntax
type ComputedField struct {
	Comment    *string                 `json:"comment,omitempty"`// Comment
	Definition ComputedFieldDefinition `json:"definition"`       // The computed field definition
	Name       string                  `json:"name"`             // Name of the new computed field
}

// The computed field definition
//
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/computed-field.html#computedfielddefinition
type ComputedFieldDefinition struct {
	Function        *FunctionName `json:"function"`                  // The SQL function
	SessionArgument *string       `json:"session_argument,omitempty"`// Name of the argument which accepts the Hasura session object as a JSON/JSONB value. If; omitted, the Hasura session object is not passed to the function
	TableArgument   *string       `json:"table_argument,omitempty"`  // Name of the argument which accepts a table row type. If omitted, the first argument is; considered a table argument
}

// Configuration for the table/view
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/table-view.html#table-config
type TableConfig struct {
	CustomColumnNames map[string]string `json:"custom_column_names,omitempty"`// Customise the column names
	CustomRootFields  *CustomRootFields `json:"custom_root_fields,omitempty"` // Customise the root fields
}

// Customise the root fields
//
// Customise the root fields
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/table-view.html#custom-root-fields
type CustomRootFields struct {
	Delete          *string `json:"delete,omitempty"`          // Customise the `delete_<table-name>` root field
	DeleteByPk      *string `json:"delete_by_pk,omitempty"`    // Customise the `delete_<table-name>_by_pk` root field
	Insert          *string `json:"insert,omitempty"`          // Customise the `insert_<table-name>` root field
	InsertOne       *string `json:"insert_one,omitempty"`      // Customise the `insert_<table-name>_one` root field
	Select          *string `json:"select,omitempty"`          // Customise the `<table-name>` root field
	SelectAggregate *string `json:"select_aggregate,omitempty"`// Customise the `<table-name>_aggregate` root field
	SelectByPk      *string `json:"select_by_pk,omitempty"`    // Customise the `<table-name>_by_pk` root field
	Update          *string `json:"update,omitempty"`          // Customise the `update_<table-name>` root field
	UpdateByPk      *string `json:"update_by_pk,omitempty"`    // Customise the `update_<table-name>_by_pk` root field
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#create-delete-permission-syntax
type DeletePermissionEntry struct {
	Comment    *string          `json:"comment,omitempty"`// Comment
	Permission DeletePermission `json:"permission"`       // The permission definition
	Role       string           `json:"role"`             // Role
}

// The permission definition
//
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#deletepermission
type DeletePermission struct {
	Filter map[string]*Filter `json:"filter,omitempty"`// Only the rows where this precondition holds true are updatable
}

// NOTE: The metadata type doesn't QUITE match the 'create' arguments here
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#create-event-trigger
type EventTrigger struct {
	Definition     EventTriggerDefinition `json:"definition"`                // The SQL function
	Headers        []Header               `json:"headers"`                   // The SQL function
	Name           string                 `json:"name"`                      // Name of the event trigger
	RetryConf      RetryConf              `json:"retry_conf"`                // The SQL function
	Webhook        *string                `json:"webhook,omitempty"`         // The SQL function
	WebhookFromEnv *string                `json:"webhook_from_env,omitempty"`
}

// The SQL function
type EventTriggerDefinition struct {
	Delete       *OperationSpec `json:"delete,omitempty"`// ; https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec
	EnableManual bool           `json:"enable_manual"`   
	Insert       *OperationSpec `json:"insert,omitempty"`// ; https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec
	Update       *OperationSpec `json:"update,omitempty"`// ; https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec
type OperationSpec struct {
	Columns *EventTriggerColumns `json:"columns"`// ; https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#eventtriggercolumns
	Payload *EventTriggerColumns `json:"payload"`// ; https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#eventtriggercolumns
}

// The SQL function
//
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#retryconf
type RetryConf struct {
	IntervalSEC *int64 `json:"interval_sec,omitempty"`// Number of seconds to wait between each retry.; Default: 10
	NumRetries  *int64 `json:"num_retries,omitempty"` // Number of times to retry delivery.; Default: 0
	TimeoutSEC  *int64 `json:"timeout_sec,omitempty"` // Number of seconds to wait for response before timing out.; Default: 60
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#args-syntax
type InsertPermissionEntry struct {
	Comment    *string          `json:"comment,omitempty"`// Comment
	Permission InsertPermission `json:"permission"`       // The permission definition
	Role       string           `json:"role"`             // Role
}

// The permission definition
//
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#insertpermission
type InsertPermission struct {
	BackendOnly *bool                `json:"backend_only,omitempty"`// When set to true the mutation is accessible only if x-hasura-use-backend-only-permissions; session variable exists; and is set to true and request is made with x-hasura-admin-secret set if any auth is; configured
	Check       map[string]*Filter   `json:"check,omitempty"`       // This expression has to hold true for every new row that is inserted
	Columns     *EventTriggerColumns `json:"columns"`               // Can insert into only these columns (or all when '*' is specified)
	Set         map[string]string    `json:"set,omitempty"`         // Preset values for columns that can be sourced from session variables or static values
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#args-syntax
type ObjectRelationship struct {
	Comment *string     `json:"comment,omitempty"`// Comment
	Name    string      `json:"name"`             // Name of the new relationship
	Using   ObjRelUsing `json:"using"`            // Use one of the available ways to define an object relationship
}

// Use one of the available ways to define an object relationship
//
// Use one of the available ways to define an object relationship
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#objrelusing
type ObjRelUsing struct {
	ForeignKeyConstraintOn *string                   `json:"foreign_key_constraint_on,omitempty"`// The column with foreign key constraint
	ManualConfiguration    *ObjRelUsingManualMapping `json:"manual_configuration,omitempty"`     // Manual mapping of table and columns
}

// Manual mapping of table and columns
//
// Manual mapping of table and columns
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#objrelusingmanualmapping
type ObjRelUsingManualMapping struct {
	ColumnMapping map[string]string `json:"column_mapping"`// Mapping of columns from current table to remote table
	RemoteTable   *TableName        `json:"remote_table"`  // The table to which the relationship has to be established
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/remote-relationships.html#args-syntax
type RemoteRelationship struct {
	Definition RemoteRelationshipDef `json:"definition"`// Definition object
	Name       string                `json:"name"`      // Name of the remote relationship
}

// Definition object
type RemoteRelationshipDef struct {
	HasuraFields []string                    `json:"hasura_fields"`// Column(s) in the table that is used for joining with remote schema field.; All join keys in remote_field must appear here.
	RemoteField  map[string]RemoteFieldValue `json:"remote_field"` // The schema tree ending at the field in remote schema which needs to be joined with.
	RemoteSchema string                      `json:"remote_schema"`// Name of the remote schema to join with
}

type RemoteFieldValue struct {
	Arguments map[string]string           `json:"arguments"`      
	Field     map[string]RemoteFieldValue `json:"field,omitempty"`// A recursive tree structure that points to the field in the remote schema that needs to be; joined with.; It is recursive because the remote field maybe nested deeply in the remote schema.; ; https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/remote-relationships.html#remotefield
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#create-select-permission-syntax
type SelectPermissionEntry struct {
	Comment    *string          `json:"comment,omitempty"`// Comment
	Permission SelectPermission `json:"permission"`       // The permission definition
	Role       string           `json:"role"`             // Role
}

// The permission definition
//
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#selectpermission
type SelectPermission struct {
	AllowAggregations *bool                `json:"allow_aggregations,omitempty"`// Toggle allowing aggregate queries
	Columns           *EventTriggerColumns `json:"columns"`                     // Only these columns are selectable (or all when '*' is specified)
	ComputedFields    []string             `json:"computed_fields"`             // Only these computed fields are selectable
	Filter            map[string]*Filter   `json:"filter,omitempty"`            // Only the rows where this precondition holds true are selectable
	Limit             *int64               `json:"limit,omitempty"`             // The maximum number of rows that can be returned
}

//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#create-update-permission-syntax
type UpdatePermissionEntry struct {
	Comment    *string          `json:"comment,omitempty"`// Comment
	Permission UpdatePermission `json:"permission"`       // The permission definition
	Role       string           `json:"role"`             // Role
}

// The permission definition
//
//
// https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#updatepermission
type UpdatePermission struct {
	Check   map[string]*Filter   `json:"check,omitempty"` // Postcondition which must be satisfied by rows which have been updated
	Columns *EventTriggerColumns `json:"columns"`         // Only these columns are selectable (or all when '*' is specified)
	Filter  map[string]*Filter   `json:"filter,omitempty"`// Only the rows where this precondition holds true are updatable
	Set     map[string]string    `json:"set,omitempty"`   // Preset values for columns that can be sourced from session variables or static values
}

type ActionDefinitionType string
const (
	Mutation ActionDefinitionType = "mutation"
	Query ActionDefinitionType = "query"
)

// Type of the relationship
type CustomTypeObjectRelationshipType string
const (
	Array CustomTypeObjectRelationshipType = "array"
	Object CustomTypeObjectRelationshipType = "object"
)

type Columns string
const (
	Empty Columns = "*"
)

type TableName struct {
	QualifiedTable *QualifiedTable
	String         *string
}

func (x *TableName) UnmarshalJSON(data []byte) error {
	x.QualifiedTable = nil
	var c QualifiedTable
	object, err := unmarshalUnion(data, nil, nil, nil, &x.String, false, nil, true, &c, false, nil, false, nil, false)
	if err != nil {
		return err
	}
	if object {
		x.QualifiedTable = &c
	}
	return nil
}

func (x *TableName) MarshalJSON() ([]byte, error) {
	return marshalUnion(nil, nil, nil, x.String, false, nil, x.QualifiedTable != nil, x.QualifiedTable, false, nil, false, nil, false)
}

type FunctionName struct {
	QualifiedFunction *QualifiedFunction
	String            *string
}

func (x *FunctionName) UnmarshalJSON(data []byte) error {
	x.QualifiedFunction = nil
	var c QualifiedFunction
	object, err := unmarshalUnion(data, nil, nil, nil, &x.String, false, nil, true, &c, false, nil, false, nil, false)
	if err != nil {
		return err
	}
	if object {
		x.QualifiedFunction = &c
	}
	return nil
}

func (x *FunctionName) MarshalJSON() ([]byte, error) {
	return marshalUnion(nil, nil, nil, x.String, false, nil, x.QualifiedFunction != nil, x.QualifiedFunction, false, nil, false, nil, false)
}

type Filter struct {
	AnythingMap map[string]interface{}
	Double      *float64
	String      *string
}

func (x *Filter) UnmarshalJSON(data []byte) error {
	x.AnythingMap = nil
	object, err := unmarshalUnion(data, nil, &x.Double, nil, &x.String, false, nil, false, nil, true, &x.AnythingMap, false, nil, false)
	if err != nil {
		return err
	}
	if object {
	}
	return nil
}

func (x *Filter) MarshalJSON() ([]byte, error) {
	return marshalUnion(nil, x.Double, nil, x.String, false, nil, false, nil, x.AnythingMap != nil, x.AnythingMap, false, nil, false)
}

type EventTriggerColumns struct {
	Enum        *Columns
	StringArray []string
}

func (x *EventTriggerColumns) UnmarshalJSON(data []byte) error {
	x.StringArray = nil
	x.Enum = nil
	object, err := unmarshalUnion(data, nil, nil, nil, nil, true, &x.StringArray, false, nil, false, nil, true, &x.Enum, false)
	if err != nil {
		return err
	}
	if object {
	}
	return nil
}

func (x *EventTriggerColumns) MarshalJSON() ([]byte, error) {
	return marshalUnion(nil, nil, nil, nil, x.StringArray != nil, x.StringArray, false, nil, false, nil, x.Enum != nil, x.Enum, false)
}

func unmarshalUnion(data []byte, pi **int64, pf **float64, pb **bool, ps **string, haveArray bool, pa interface{}, haveObject bool, pc interface{}, haveMap bool, pm interface{}, haveEnum bool, pe interface{}, nullable bool) (bool, error) {
	if pi != nil {
		*pi = nil
	}
	if pf != nil {
		*pf = nil
	}
	if pb != nil {
		*pb = nil
	}
	if ps != nil {
		*ps = nil
	}

	dec := json.NewDecoder(bytes.NewReader(data))
	dec.UseNumber()
	tok, err := dec.Token()
	if err != nil {
		return false, err
	}

	switch v := tok.(type) {
	case json.Number:
		if pi != nil {
			i, err := v.Int64()
			if err == nil {
				*pi = &i
				return false, nil
			}
		}
		if pf != nil {
			f, err := v.Float64()
			if err == nil {
				*pf = &f
				return false, nil
			}
			return false, errors.New("Unparsable number")
		}
		return false, errors.New("Union does not contain number")
	case float64:
		return false, errors.New("Decoder should not return float64")
	case bool:
		if pb != nil {
			*pb = &v
			return false, nil
		}
		return false, errors.New("Union does not contain bool")
	case string:
		if haveEnum {
			return false, json.Unmarshal(data, pe)
		}
		if ps != nil {
			*ps = &v
			return false, nil
		}
		return false, errors.New("Union does not contain string")
	case nil:
		if nullable {
			return false, nil
		}
		return false, errors.New("Union does not contain null")
	case json.Delim:
		if v == '{' {
			if haveObject {
				return true, json.Unmarshal(data, pc)
			}
			if haveMap {
				return false, json.Unmarshal(data, pm)
			}
			return false, errors.New("Union does not contain object")
		}
		if v == '[' {
			if haveArray {
				return false, json.Unmarshal(data, pa)
			}
			return false, errors.New("Union does not contain array")
		}
		return false, errors.New("Cannot handle delimiter")
	}
	return false, errors.New("Cannot unmarshal union")

}

func marshalUnion(pi *int64, pf *float64, pb *bool, ps *string, haveArray bool, pa interface{}, haveObject bool, pc interface{}, haveMap bool, pm interface{}, haveEnum bool, pe interface{}, nullable bool) ([]byte, error) {
	if pi != nil {
		return json.Marshal(*pi)
	}
	if pf != nil {
		return json.Marshal(*pf)
	}
	if pb != nil {
		return json.Marshal(*pb)
	}
	if ps != nil {
		return json.Marshal(*ps)
	}
	if haveArray {
		return json.Marshal(pa)
	}
	if haveObject {
		return json.Marshal(pc)
	}
	if haveMap {
		return json.Marshal(pm)
	}
	if haveEnum {
		return json.Marshal(pe)
	}
	if nullable {
		return json.Marshal(nil)
	}
	return nil, errors.New("Union must not be null")
}
