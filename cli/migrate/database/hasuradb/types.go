package hasuradb

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/mitchellh/mapstructure"

	"github.com/hasura/graphql-engine/cli/migrate/database"

	"github.com/qor/transition"
)

const (
	TuplesOK  = "TuplesOk"
	CommandOK = "CommandOk"
)

type HasuraInterfaceBulk struct {
	Type string        `json:"type" yaml:"type"`
	Args []interface{} `json:"args" yaml:"args"`
}

func (h *HasuraInterfaceBulk) ResetArgs() {
	h.Args = make([]interface{}, 0)
}

type HasuraInterfaceQuery struct {
	Type    requestTypes    `json:"type" yaml:"type"`
	Version metadataVersion `json:"version,omitempty" yaml:"version,omitempty"`
	Source  string          `json:"source,omitempty" yaml:"source,omitempty"`
	Args    interface{}     `json:"args" yaml:"args"`
}

type metadataVersion int

const (
	v1 metadataVersion = 1
	v2                 = 2
	v3                 = 3
)

type newHasuraIntefaceQuery struct {
	Type    requestTypes    `json:"type" yaml:"type"`
	Version metadataVersion `json:"version,omitempty" yaml:"version,omitempty"`
	Args    interface{}     `json:"args" yaml:"args"`
}

type deleteRemoteRelationshipInput struct {
	Name  string      `json:"name" yaml:"name"`
	Table tableSchema `json:"table" yaml:"table"`
}

type remoteRelationshipDefinition struct {
	HasuraFields []string               `yaml:"hasura_fields" json:"hasura_fields"`
	Name         string                 `yaml:"name" json:"name"`
	RemoteField  map[string]interface{} `yaml:"remote_field" json:"remote_field"`
	RemoteSchema string                 `yaml:"remote_schema" json:"remote_schema"`
}
type createRemoteRelationshipInput struct {
	remoteRelationshipDefinition
	Table tableSchema `yaml:"table" json:"table"`
}
type updateRemoteRelationshipInput struct {
	*createRemoteRelationshipInput
}

type createCronTriggerInput struct {
	Name              string      `json:"name" yaml:"name"`
	Webhook           string      `json:"webhook" yaml:"webhook"`
	Schedule          string      `json:"schedule" yaml:"schedule"`
	Payload           interface{} `json:"payload,omitempty" yaml:"payload,omitempty"`
	Headers           interface{} `json:"headers,omitempty" yaml:"headers,omitempty"`
	RetryConf         interface{} `json:"retry_conf,omitempty" yaml:"retry_conf,omitempty"`
	IncludeInMetadata *bool       `json:"include_in_metadata,omitempty" yaml:"include_in_metadata,omitempty"`
	Comment           string      `json:"comment,omitempty" yaml:"comment,omitempty"`
	Replace           *bool       `json:"replace,omitempty" yaml:"replace,omitempty"`
}

type deleteCronTriggerInput struct {
	Name string `json:"name" yaml:"name"`
}

type actionDefinition struct {
	Name       interface{} `json:"name,omitempty" yaml:"name,omitempty"`
	Definition interface{} `json:"definition,omitempty" yaml:"definition,omitempty"`
}

type createActionInput struct {
	actionDefinition
	Comment string `json:"comment,omitempty" yaml:"comment,omitempty"`
}

type actionAndPermission struct {
	actionDefinition
	Permissions []PermissionDefinition `json:"permissions" yaml:"permissions"`
}

type dropActionInput struct {
	Name      interface{} `json:"name,omitempty" yaml:"name,omitempty"`
	ClearData bool        `json:"clear_data,omitempty" yaml:"clear_data,omitempty"`
}

type updateActionInput struct {
	actionDefinition
}

type PermissionDefinition struct {
	Role    interface{} `json:"role,omitempty" yaml:"role,omitempty"`
	Comment string      `json:"comment,omitempty" yaml:"comment,omitempty"`
}
type createActionPermissionInput struct {
	Action interface{} `json:"action,omitempty" yaml:"action,omitempty"`
	PermissionDefinition
}

type dropActionPermissionInput struct {
	Action interface{} `json:"action,omitempty" yaml:"action,omitempty"`
	PermissionDefinition
}

type setCustomTypesInput struct {
	InputObjects interface{} `json:"input_objects,omitempty" yaml:"input_objects,omitempty"`
	Objects      interface{} `json:"objects,omitempty" yaml:"objects,omitempty"`
	Scalars      interface{} `json:"scalars,omitempty" yaml:"scalars,omitempty"`
	Enums        interface{} `json:"enums,omitempty" yaml:"enums,omitempty"`
}

func (h *newHasuraIntefaceQuery) UnmarshalJSON(b []byte) error {
	type t newHasuraIntefaceQuery
	var q t
	if err := json.Unmarshal(b, &q); err != nil {
		return err
	}
	if q.Version == 0 {
		q.Version = v1
	}
	argBody, err := json.Marshal(q.Args)
	if err != nil {
		return err
	}
	switch q.Type {
	case trackTable, addExistingTableOrView:
		switch q.Version {
		case v2:
			q.Args = &trackTableV2Input{}
		default:
			q.Args = &trackTableInput{}
		}
	case setTableCustomFields:
		q.Args = &setTableCustomFieldsV2Input{}
	case setTableCustomization:
		q.Args = &setTableCustomizationInput{}
	case setTableIsEnum:
		q.Args = &setTableIsEnumInput{}
	case untrackTable:
		q.Args = &unTrackTableInput{}
	case createObjectRelationship:
		q.Args = &createObjectRelationshipInput{}
	case createArrayRelationship:
		q.Args = &createArrayRelationshipInput{}
	case setRelationshipComment:
		q.Args = &setRelationshipCommentInput{}
	case dropRelationship:
		q.Args = &dropRelationshipInput{}
	case createInsertPermission:
		q.Args = &createInsertPermissionInput{}
	case dropInsertPermission:
		q.Args = &dropInsertPermissionInput{}
	case createSelectPermission:
		q.Args = &createSelectPermissionInput{}
	case dropSelectPermission:
		q.Args = &dropSelectPermissionInput{}
	case createUpdatePermission:
		q.Args = &createUpdatePermissionInput{}
	case dropUpdatePermission:
		q.Args = &dropUpdatePermissionInput{}
	case createDeletePermission:
		q.Args = &createDeletePermissionInput{}
	case dropDeletePermission:
		q.Args = &dropDeletePermissionInput{}
	case trackFunction:
		q.Args = &trackFunctionInput{}
	case unTrackFunction:
		q.Args = &unTrackFunctionInput{}
	case createEventTrigger:
		q.Args = &createEventTriggerInput{}
	case deleteEventTrigger:
		q.Args = &deleteEventTriggerInput{}
	case addRemoteSchema:
		q.Args = &addRemoteSchemaInput{}
	case removeRemoteSchema:
		q.Args = &removeRemoteSchemaInput{}
	case createQueryCollection:
		q.Args = &createQueryCollectionInput{}
	case dropQueryCollection:
		q.Args = &dropQueryCollectionInput{}
	case addQueryToCollection:
		q.Args = &addQueryToCollectionInput{}
	case dropQueryFromCollection:
		q.Args = &dropQueryFromCollectionInput{}
	case addCollectionToAllowList:
		q.Args = &addCollectionToAllowListInput{}
	case dropCollectionFromAllowList:
		q.Args = &dropCollectionFromAllowListInput{}
	case replaceMetadata:
		q.Args = &replaceMetadataInput{}
	case clearMetadata:
		q.Args = &clearMetadataInput{}
	case RunSQL:
		q.Args = &RunSQLInput{}
	case addComputedField:
		q.Args = &addComputedFieldInput{}
	case dropComputedField:
		q.Args = &dropComputedFieldInput{}
	case deleteRemoteRelationship:
		q.Args = &deleteRemoteRelationshipInput{}
	case createRemoteRelationship:
		q.Args = &createRemoteRelationshipInput{}
	case updateRemoteRelationship:
		q.Args = &createRemoteRelationshipInput{}
	case createCronTrigger:
		q.Args = &createCronTriggerInput{}
	case deleteCronTrigger:
		q.Args = &deleteCronTriggerInput{}
	case createAction:
		q.Args = &createActionInput{}
	case dropAction:
		q.Args = &dropActionInput{}
	case updateAction:
		q.Args = &updateActionInput{}
	case createActionPermission:
		q.Args = &createActionPermissionInput{}
	case dropActionPermission:
		q.Args = &dropActionPermissionInput{}
	case setCustomTypes:
		q.Args = &setCustomTypesInput{}
	default:
		return fmt.Errorf("cannot squash type %s", q.Type)
	}
	if err := json.Unmarshal(argBody, &q.Args); err != nil {
		return err
	}
	if q.Args == nil {
		return fmt.Errorf("args is missing in metadata action %s", q.Type)
	}
	*h = newHasuraIntefaceQuery(q)
	return nil
}

type HasuraQuery struct {
	Type string     `json:"type" yaml:"type"`
	Args HasuraArgs `json:"args" yaml:"args"`
}

type HasuraBulk struct {
	Type string        `json:"type" yaml:"type"`
	Args []HasuraQuery `json:"args" yaml:"args"`
}

type HasuraArgs struct {
	SQL       string        `json:"sql,omitempty" yaml:"sql"`
	Table     interface{}   `json:"table,omitempty"`
	Columns   interface{}   `json:"columns,omitempty"`
	Where     interface{}   `json:"where,omitempty"`
	OrderBy   interface{}   `json:"order_by,omitempty"`
	Objects   []interface{} `json:"objects,omitempty"`
	Limit     int           `json:"limit,omitempty"`
	Returning []string      `json:"returning,omitempty"`
	Set       interface{}   `json:"$set,omitempty"`
}

type HasuraOrderBy struct {
	Column string `json:"column,omitempty"`
	Type   string `json:"type,omitempty"`
	Nulls  string `json:"nulls,omitempty"`
}

type HasuraColumn struct {
	Name    string      `json:"name"`
	Columns interface{} `json:"columns,omitempty"`
}

type HasuraError struct {
	// MigrationFile is used internally for hasuractl
	migrationFile  string
	migrationQuery string
	Path           string      `json:"path"`
	ErrorMessage   string      `json:"error"`
	Internal       interface{} `json:"internal,omitempty"`
	Message        string      `json:"message,omitempty"`
	Code           string      `json:"code"`
}

type InconsistentMetadataError struct {
	Definition interface{} `json:"definition,omitempty" mapstructure:"definition,omitempty"`
	Reason     string      `json:"reason,omitempty" mapstructure:"reason,omitempty"`
	Type       string      `json:"type,omitempty" mapstructure:"type,omitempty"`
}

func (mderror *InconsistentMetadataError) String() string {
	var out string
	if mderror.Reason != "" {
		out = fmt.Sprintf("\nreason: %v\n", mderror.Reason)
	}
	if mderror.Type != "" {
		out = fmt.Sprintf("%stype: %v\n", out, mderror.Type)
	}
	if mderror.Definition != nil {
		m, err := json.MarshalIndent(mderror.Definition, "", "  ")
		if err == nil {
			out = fmt.Sprintf("%sdefinition: \n%s", out, string(m))
		}
	}
	return out
}

type SQLInternalError struct {
	Arguments                 []string       `json:"arguments" mapstructure:"arguments,omitempty"`
	Error                     *PostgresError `json:"error" mapstructure:"error,omitempty"`
	Prepared                  bool           `json:"prepared" mapstructure:"prepared,omitempty"`
	Statement                 string         `json:"statement" mapstructure:"statement,omitempty"`
	InconsistentMetadataError `mapstructure:",squash"`
}
type PostgresError struct {
	StatusCode  string `json:"status_code" mapstructure:"status_code,omitempty"`
	ExecStatus  string `json:"exec_status" mapstructure:"exec_status,omitempty"`
	Message     string `json:"message" mapstructure:"message,omitempty"`
	Description string `json:"description" mapstructure:"description,omitempty"`
	Hint        string `json:"hint" mapstructure:"hint,omitempty"`
}

type SchemaDump struct {
	Opts        []string `json:"opts"`
	CleanOutput bool     `json:"clean_output"`
}

func (h HasuraError) Error() string {
	var errorStrings []string
	errorStrings = append(errorStrings, fmt.Sprintf("[%s] %s (%s)", h.Code, h.ErrorMessage, h.Path))
	if h.migrationFile != "" {
		errorStrings = append(errorStrings, fmt.Sprintf("File: '%s'", h.migrationFile))
	}
	if h.migrationQuery != "" {
		errorStrings = append(errorStrings, fmt.Sprintf("%s", h.migrationQuery))
	}
	var internalError SQLInternalError
	var internalErrors []SQLInternalError
	if v, ok := h.Internal.(map[string]interface{}); ok {
		err := mapstructure.Decode(v, &internalError)
		if err == nil {
			// postgres error
			if internalError.Error != nil {
				errorStrings = append(errorStrings, fmt.Sprintf("[%s] %s: %s", internalError.Error.StatusCode, internalError.Error.ExecStatus, internalError.Error.Message))
				if len(internalError.Error.Description) > 0 {
					errorStrings = append(errorStrings, fmt.Sprintf("Description: %s", internalError.Error.Description))
				}
				if len(internalError.Error.Hint) > 0 {
					errorStrings = append(errorStrings, fmt.Sprintf("Hint: %s", internalError.Error.Hint))
				}
			}
			if e := internalError.InconsistentMetadataError.String(); e != "" {
				errorStrings = append(errorStrings, e)
			}
		}
	}
	if v, ok := h.Internal.([]interface{}); ok {
		err := mapstructure.Decode(v, &internalErrors)
		if err == nil {
			for _, internalError := range internalErrors {
				// postgres error
				if internalError.Error != nil {
					errorStrings = append(errorStrings, fmt.Sprintf("[%s] %s: %s", internalError.Error.StatusCode, internalError.Error.ExecStatus, internalError.Error.Message))
					if len(internalError.Error.Description) > 0 {
						errorStrings = append(errorStrings, fmt.Sprintf("Description: %s", internalError.Error.Description))
					}
					if len(internalError.Error.Hint) > 0 {
						errorStrings = append(errorStrings, fmt.Sprintf("Hint: %s", internalError.Error.Hint))
					}
				}

				if e := internalError.InconsistentMetadataError.String(); e != "" {
					errorStrings = append(errorStrings, e)
				}
			}
		}
	}
	if len(errorStrings) == 0 {
		return ""
	}
	return strings.Join(errorStrings, "\r\n")
}

// NewHasuraError - returns error based on data and isCmd
func NewHasuraError(data []byte, isCmd bool) error {
	switch isCmd {
	case true:
		var herror HasuraError
		err := json.Unmarshal(data, &herror)
		if err != nil {
			return fmt.Errorf("failed parsing json: %v; response from API: %s", err, string(data))
		}
		return herror
	default:
		return fmt.Errorf("Data Error: %s", string(data))
	}
}

type HasuraSQLRes struct {
	ResultType string     `json:"result_type"`
	Result     [][]string `json:"result"`
}

type requestTypes string

const (
	trackTable                  requestTypes = "track_table"
	addExistingTableOrView                   = "add_existing_table_or_view"
	setTableCustomFields                     = "set_table_custom_fields"
	setTableCustomization                    = "set_table_customization"
	setTableIsEnum                           = "set_table_is_enum"
	untrackTable                             = "untrack_table"
	trackFunction                            = "track_function"
	unTrackFunction                          = "untrack_function"
	createObjectRelationship                 = "create_object_relationship"
	createArrayRelationship                  = "create_array_relationship"
	dropRelationship                         = "drop_relationship"
	setRelationshipComment                   = "set_relationship_comment"
	createInsertPermission                   = "create_insert_permission"
	dropInsertPermission                     = "drop_insert_permission"
	createSelectPermission                   = "create_select_permission"
	dropSelectPermission                     = "drop_select_permission"
	createUpdatePermission                   = "create_update_permission"
	dropUpdatePermission                     = "drop_update_permission"
	createDeletePermission                   = "create_delete_permission"
	dropDeletePermission                     = "drop_delete_permission"
	setPermissionComment                     = "set_permission_comment"
	createEventTrigger                       = "create_event_trigger"
	deleteEventTrigger                       = "delete_event_trigger"
	addRemoteSchema                          = "add_remote_schema"
	removeRemoteSchema                       = "remove_remote_schema"
	createQueryCollection                    = "create_query_collection"
	dropQueryCollection                      = "drop_query_collection"
	addQueryToCollection                     = "add_query_to_collection"
	dropQueryFromCollection                  = "drop_query_from_collection"
	addCollectionToAllowList                 = "add_collection_to_allowlist"
	dropCollectionFromAllowList              = "drop_collection_from_allowlist"
	replaceMetadata                          = "replace_metadata"
	clearMetadata                            = "clear_metadata"
	RunSQL                                   = "run_sql"
	bulkQuery                                = "bulk"
	addComputedField                         = "add_computed_field"
	dropComputedField                        = "drop_computed_field"
	createRemoteRelationship                 = "create_remote_relationship"
	updateRemoteRelationship                 = "update_remote_relationship"
	deleteRemoteRelationship                 = "delete_remote_relationship"
	createCronTrigger                        = "create_cron_trigger"
	deleteCronTrigger                        = "delete_cron_trigger"
	createAction                             = "create_action"
	dropAction                               = "drop_action"
	updateAction                             = "update_action"
	createActionPermission                   = "create_action_permission"
	dropActionPermission                     = "drop_action_permission"
	setCustomTypes                           = "set_custom_types"
)

type tableMap struct {
	name, schema string
}

type relationshipMap struct {
	tableName, schemaName, name string
}

type permissionMap struct {
	tableName, schemaName, permType, Role string
}

type computedFieldMap struct {
	tableName, schemaName, name string
}

type queryInCollectionMap struct {
	collectionName, queryName string
}

type remoteRelationshipMap struct {
	tableName, schemaName, name string
}

type tableSchema struct {
	Name   string `json:"name" yaml:"name"`
	Schema string `json:"schema" yaml:"schema"`
}

func (t *tableSchema) UnmarshalJSON(b []byte) error {
	var table string
	if err := json.Unmarshal(b, &table); err != nil {
		var ts struct {
			Name   string `json:"name"`
			Schema string `json:"schema"`
		}
		if err := json.Unmarshal(b, &ts); err != nil {
			return err
		}
		t.Name = ts.Name
		t.Schema = ts.Schema
		return nil
	}
	t.Name = table
	t.Schema = "public"
	return nil
}

type trackTableInput struct {
	tableSchema
	IsEnum bool `json:"is_enum" yaml:"is_enum"`
}

func (t *trackTableInput) UnmarshalJSON(b []byte) error {
	type tmpT trackTableInput
	var ts tmpT
	if err := json.Unmarshal(b, &ts); err != nil {
		return err
	}
	if ts.Schema == "" {
		ts.Schema = "public"
	}
	*t = trackTableInput(ts)
	return nil
}

type tableConfiguration struct {
	CustomName        string            `json:"custom_name,omitempty" yaml:"custom_name,omitempty"`
	CustomRootFields  map[string]string `json:"custom_root_fields,omitempty" yaml:"custom_root_fields,omitempty"`
	CustomColumnNames map[string]string `json:"custom_column_names,omitempty" yaml:"custom_column_names,omitempty"`
}

type trackTableV2Input struct {
	Table         tableSchema        `json:"table" yaml:"table"`
	Configuration tableConfiguration `json:"configuration" yaml:"configuration"`
}

type setTableCustomFieldsV2Input struct {
	Table tableSchema `json:"table" yaml:"table"`
	tableConfiguration
}

type setTableCustomizationInput struct {
	Table              tableSchema `json:"table" yaml:"table"`
	tableConfiguration `json:"configuration,omitempty" yaml:"configuration,omitempty"`
}

type setTableIsEnumInput struct {
	Table  tableSchema `json:"table" yaml:"table"`
	IsEnum bool        `json:"is_enum" yaml:"is_enum"`
}

type unTrackTableInput struct {
	tableSchema
}

func (t *unTrackTableInput) UnmarshalJSON(b []byte) error {
	type tmpT unTrackTableInput
	var ts tmpT
	if err := json.Unmarshal(b, &ts); err != nil {
		return err
	}
	if ts.Schema == "" {
		ts.Schema = "public"
	}
	*t = unTrackTableInput(ts)
	return nil
}

type trackFunctionInput struct {
	tableSchema
}

type unTrackFunctionInput struct {
	Schema string `json:"schema" yaml:"schema"`
	Name   string `json:"name" yaml:"name"`
}

type createObjectRelationshipInput struct {
	Name    string      `json:"name" yaml:"name"`
	Table   tableSchema `json:"table" yaml:"table"`
	Using   interface{} `json:"using" yaml:"using"`
	Comment *string     `json:"comment,omitempty" yaml:"comment,omitempty"`
}

type createArrayRelationshipInput struct {
	Name    string      `json:"name" yaml:"name"`
	Table   tableSchema `json:"table" yaml:"table"`
	Using   interface{} `json:"using" yaml:"using"`
	Comment *string     `json:"comment,omitempty" yaml:"comment,omitempty"`
}

type setRelationshipCommentInput struct {
	Name    string      `json:"name" yaml:"name"`
	Table   tableSchema `json:"table" yaml:"table"`
	Comment *string     `json:"comment" yaml:"comment"`
}

type dropRelationshipInput struct {
	RelationShip string      `json:"relationship" yaml:"relationship"`
	Table        tableSchema `json:"table" yaml:"table"`
}

type createInsertPermissionInput struct {
	Table      tableSchema `json:"table" yaml:"table"`
	Role       string      `json:"role" yaml:"role"`
	Permission interface{} `json:"permission" yaml:"permission"`
	Comment    *string     `json:"comment,omitempty" yaml:"comment,omitempty"`
}

type dropInsertPermissionInput struct {
	Table tableSchema `json:"table" yaml:"table"`
	Role  string      `json:"role" yaml:"role"`
}

type createSelectPermissionInput struct {
	Table      tableSchema `json:"table" yaml:"table"`
	Role       string      `json:"role" yaml:"role"`
	Permission interface{} `json:"permission" yaml:"permission"`
	Comment    *string     `json:"comment,omitempty" yaml:"comment,omitempty"`
}

type dropSelectPermissionInput struct {
	Table tableSchema `json:"table" yaml:"table"`
	Role  string      `json:"role" yaml:"role"`
}

type createUpdatePermissionInput struct {
	Table      tableSchema `json:"table" yaml:"table"`
	Role       string      `json:"role" yaml:"role"`
	Permission interface{} `json:"permission" yaml:"permission"`
	Comment    *string     `json:"comment,omitempty" yaml:"comment,omitempty"`
}

type dropUpdatePermissionInput struct {
	Table tableSchema `json:"table" yaml:"table"`
	Role  string      `json:"role" yaml:"role"`
}

type createDeletePermissionInput struct {
	Table      tableSchema `json:"table" yaml:"table"`
	Role       string      `json:"role" yaml:"role"`
	Permission interface{} `json:"permission" yaml:"permission"`
	Comment    *string     `json:"comment,omitempty" yaml:"comment,omitempty"`
}

type dropDeletePermissionInput struct {
	Table tableSchema `json:"table" yaml:"table"`
	Role  string      `json:"role" yaml:"role"`
}

type setPermissionCommentInput struct {
	Table   tableSchema `json:"table" yaml:"table"`
	Role    string      `json:"role" yaml:"role"`
	Type    string      `json:"type" yaml:"type"`
	Comment *string     `json:"comment" yaml:"comment"`
}

type createEventTriggerInput struct {
	Name           string                            `json:"name" yaml:"name"`
	Table          tableSchema                       `json:"table" yaml:"table"`
	Webhook        string                            `json:"webhook,omitempty" yaml:"webhook,omitempty"`
	WebhookFromEnv string                            `json:"webhook_from_env,omitempty" yaml:"webhook_from_env,omitempty"`
	Definition     *createEventTriggerOperationInput `json:"definition,omitempty" yaml:"definition,omitempty"`
	Headers        interface{}                       `json:"headers" yaml:"headers"`
	Replace        *bool                             `json:"replace,omitempty" yaml:"replace,omitempty"`
	RetryConf      *createEventTriggerRetryConfInput `json:"retry_conf" yaml:"retry_conf"`
	EnableManual   *bool                             `json:"enable_manual,omitempty" yaml:"enable_manual,omitempty"`

	createEventTriggerOperationInput
}

type createEventTriggerRetryConfInput struct {
	IntervalSec int `json:"interval_sec" yaml:"interval_sec"`
	NumRetries  int `json:"num_retries" yaml:"num_retries"`
	TimeOutSec  int `json:"timeout_sec" yaml:"timeout_sec"`
}

type createEventTriggerOperationInput struct {
	Insert interface{} `json:"insert,omitempty" yaml:"insert,omitempty"`
	Update interface{} `json:"update,omitempty" yaml:"update,omitempty"`
	Delete interface{} `json:"delete,omitempty" yaml:"delete,omitempty"`
}

func (c *createEventTriggerInput) MarshalJSON() ([]byte, error) {
	if c.Definition != nil {
		c.Insert = c.Definition.Insert
		c.Update = c.Definition.Update
		c.Delete = c.Definition.Delete
		c.Definition = nil
	}
	return json.Marshal(&struct {
		Name           string                            `json:"name" yaml:"name"`
		Table          tableSchema                       `json:"table" yaml:"table"`
		Webhook        string                            `json:"webhook,omitempty" yaml:"webhook,omitempty"`
		WebhookFromEnv string                            `json:"webhook_from_env,omitempty" yaml:"webhook_from_env,omitempty"`
		Headers        interface{}                       `json:"headers" yaml:"headers"`
		Replace        *bool                             `json:"replace,omitempty" yaml:"replace,omitempty"`
		RetryConf      *createEventTriggerRetryConfInput `json:"retry_conf" yaml:"retry_conf"`
		Insert         interface{}                       `json:"insert,omitempty" yaml:"insert,omitempty"`
		Update         interface{}                       `json:"update,omitempty" yaml:"update,omitempty"`
		Delete         interface{}                       `json:"delete,omitempty" yaml:"delete,omitempty"`
		EnableManual   *bool                             `json:"enable_manual,omitempty" yaml:"enable_manual,omitempty"`
	}{
		Name:           c.Name,
		Table:          c.Table,
		Webhook:        c.Webhook,
		WebhookFromEnv: c.WebhookFromEnv,
		Headers:        c.Headers,
		Replace:        c.Replace,
		RetryConf:      c.RetryConf,
		Insert:         c.Insert,
		Update:         c.Update,
		Delete:         c.Delete,
		EnableManual:   c.EnableManual,
	})
}

type deleteEventTriggerInput struct {
	Name string `json:"name" yaml:"name"`
}

type addRemoteSchemaInput struct {
	Name       string                 `json:"name" yaml:"name"`
	Definition map[string]interface{} `json:"definition" yaml:"definition"`
	Comment    *string                `json:"comment,omitempty" yaml:"comment,omitempty"`
}

type removeRemoteSchemaInput struct {
	Name string `json:"name" yaml:"name"`
}

type collectionQuery struct {
	Name  string `json:"name" yaml:"name"`
	Query string `json:"query" yaml:"query"`
}

type createQueryCollectionInput struct {
	Name       string  `json:"name" yaml:"name"`
	Comment    *string `json:"comment,omitempty" yaml:"comment,omitempty"`
	Definition struct {
		Queries []collectionQuery `json:"queries" yaml:"queries"`
	} `json:"definition" yaml:"definition"`
}

type dropQueryCollectionInput struct {
	Collection string `json:"name" yaml:"name"`
	Cascade    bool   `json:"cascade" yaml:"cascade"`
}

type addQueryToCollectionInput struct {
	CollectionName string `json:"collection_name" yaml:"collection_name"`
	QueryName      string `json:"query_name" yaml:"query_name"`
	Query          string `json:"query" yaml:"query"`
}

type dropQueryFromCollectionInput struct {
	CollectionName string `json:"collection_name" yaml:"collection_name"`
	QueryName      string `json:"query_name" yaml:"query_name"`
}

type addCollectionToAllowListInput struct {
	Collection string `json:"collection" yaml:"collection"`
}

type dropCollectionFromAllowListInput struct {
	Collection string `json:"collection" yaml:"collection"`
}

type addComputedFieldInput struct {
	Table      tableSchema `json:"table" yaml:"table"`
	Name       string      `json:"name" yaml:"name"`
	Definition interface{} `json:"definition" yaml:"definition"`
}

type dropComputedFieldInput struct {
	Table   tableSchema `json:"table" yaml:"table"`
	Name    string      `json:"name" yaml:"name"`
	Cascade bool        `json:"cascade" yaml:"cascade"`
	Comment string      `json:"comment" yaml:"comment"`
}

type clearMetadataInput struct {
}

type remoteRelationships []struct {
	Definiton remoteRelationshipDefinition `json:"definiton" yaml:"definiton"`
}
type replaceMetadataInput struct {
	Tables []struct {
		Table               tableSchema                      `json:"table" yaml:"table"`
		ArrayRelationships  []*createArrayRelationshipInput  `json:"array_relationships" yaml:"array_relationships"`
		ObjectRelationships []*createObjectRelationshipInput `json:"object_relationships" yaml:"object_relationships"`
		InsertPermissions   []*createInsertPermissionInput   `json:"insert_permissions" yaml:"insert_permissions"`
		SelectPermissions   []*createSelectPermissionInput   `json:"select_permissions" yaml:"select_permissions"`
		UpdatePermissions   []*createUpdatePermissionInput   `json:"update_permissions" yaml:"update_permissions"`
		DeletePermissions   []*createDeletePermissionInput   `json:"delete_permissions" yaml:"delete_permissions"`
		EventTriggers       []*createEventTriggerInput       `json:"event_triggers" yaml:"event_triggers"`
		ComputedFields      []*addComputedFieldInput         `json:"computed_fields" yaml:"computed_fields"`
		RemoteRelationships *remoteRelationships             `json:"remote_relationships" yaml:"remote_relationships"`
		Configuration       *tableConfiguration              `json:"configuration" yaml:"configuration"`
	} `json:"tables" yaml:"tables"`
	Functions        []*trackFunctionInput            `json:"functions" yaml:"functions"`
	QueryCollections []*createQueryCollectionInput    `json:"query_collections" yaml:"query_collections"`
	AllowList        []*addCollectionToAllowListInput `json:"allowlist" yaml:"allowlist"`
	RemoteSchemas    []*addRemoteSchemaInput          `json:"remote_schemas" yaml:"remote_schemas"`
	CronTriggers     []*createCronTriggerInput        `json:"cron_triggers" yaml:"cron_triggers"`
	Actions          []*actionAndPermission           `json:"actions" yaml:"actions"`
	CustomTypes      *setCustomTypesInput             `json:"custom_types" yaml:"custom_types"`
}

func (rmi *replaceMetadataInput) convertToMetadataActions(l *database.CustomList) {
	// track tables
	for _, table := range rmi.Tables {
		if table.Configuration == nil {
			t := &trackTableInput{
				tableSchema: tableSchema{
					Name:   table.Table.Name,
					Schema: table.Table.Schema,
				},
			}
			l.PushBack(t)
		} else {
			t := &trackTableV2Input{
				Table: tableSchema{
					Name:   table.Table.Name,
					Schema: table.Table.Schema,
				},
				Configuration: *table.Configuration,
			}
			l.PushBack(t)
		}
	}

	for _, table := range rmi.Tables {
		for _, objRel := range table.ObjectRelationships {
			objRel.Table = tableSchema{
				table.Table.Name,
				table.Table.Schema,
			}
			l.PushBack(objRel)
		}
	}

	for _, table := range rmi.Tables {
		for _, arrayRel := range table.ArrayRelationships {
			arrayRel.Table = tableSchema{
				table.Table.Name,
				table.Table.Schema,
			}
			l.PushBack(arrayRel)
		}
	}

	for _, table := range rmi.Tables {
		for _, insertPerm := range table.InsertPermissions {
			insertPerm.Table = tableSchema{
				table.Table.Name,
				table.Table.Schema,
			}
			l.PushBack(insertPerm)
		}
	}

	for _, table := range rmi.Tables {
		for _, selectPerm := range table.SelectPermissions {
			selectPerm.Table = tableSchema{
				table.Table.Name,
				table.Table.Schema,
			}
			l.PushBack(selectPerm)
		}
	}

	for _, table := range rmi.Tables {
		for _, updatePerm := range table.UpdatePermissions {
			updatePerm.Table = tableSchema{
				table.Table.Name,
				table.Table.Schema,
			}
			l.PushBack(updatePerm)
		}
	}

	for _, table := range rmi.Tables {
		for _, deletePerm := range table.DeletePermissions {
			deletePerm.Table = tableSchema{
				table.Table.Name,
				table.Table.Schema,
			}
			l.PushBack(deletePerm)
		}
	}

	for _, table := range rmi.Tables {
		for _, et := range table.EventTriggers {
			et.Table = tableSchema{
				table.Table.Name,
				table.Table.Schema,
			}
			l.PushBack(et)
		}
	}

	for _, table := range rmi.Tables {
		for _, cf := range table.ComputedFields {
			cf.Table = tableSchema{
				table.Table.Name,
				table.Table.Schema,
			}
			l.PushBack(cf)
		}
	}

	for _, table := range rmi.Tables {
		if table.RemoteRelationships != nil {
			for _, remoteRelationship := range *table.RemoteRelationships {
				r := createRemoteRelationshipInput{
					remoteRelationshipDefinition: remoteRelationship.Definiton,
					Table: tableSchema{
						Name:   table.Table.Name,
						Schema: table.Table.Schema,
					},
				}
				l.PushBack(r)
			}
		}
	}

	// track functions
	for _, function := range rmi.Functions {
		l.PushBack(function)
	}

	// track query collections
	for _, qc := range rmi.QueryCollections {
		l.PushBack(qc)
	}

	// track allow list
	for _, al := range rmi.AllowList {
		l.PushBack(al)
	}

	// track remote schemas
	for _, rs := range rmi.RemoteSchemas {
		l.PushBack(rs)
	}

	// track cron triggers
	for _, ct := range rmi.CronTriggers {
		l.PushBack(ct)
	}

	// track actions
	for _, action := range rmi.Actions {
		// action definition
		a := &createActionInput{
			actionDefinition: action.actionDefinition,
		}
		l.PushBack(a)
		// permission
		for _, permission := range action.Permissions {
			p := &createActionPermissionInput{
				Action:               action.Name,
				PermissionDefinition: permission,
			}
			l.PushBack(p)
		}
	}
	if rmi.CustomTypes != nil {
		l.PushBack(rmi.CustomTypes)
	}
}

type InconsistentMetadata struct {
	IsConsistent        bool                          `json:"is_consistent"`
	InConsistentObjects []InconsistentMeatadataObject `json:"inconsistent_objects"`
}

type InconsistentMeatadataObject struct {
	Type       string      `json:"type"`
	Reason     string      `json:"reason"`
	Definition interface{} `json:"definition"`
}

func (i *InconsistentMeatadataObject) UnmarshalJSON(b []byte) error {
	type t InconsistentMeatadataObject
	var q t
	if err := json.Unmarshal(b, &q); err != nil {
		return err
	}
	defBody, err := json.Marshal(q.Definition)
	if err != nil {
		return err
	}
	switch q.Type {
	case "object_relation":
		q.Definition = &createObjectRelationshipInput{}
	case "array_relation":
		q.Definition = &createArrayRelationshipInput{}
	case "select_permission":
		q.Definition = &createSelectPermissionInput{}
	case "update_permission":
		q.Definition = &createUpdatePermissionInput{}
	case "insert_permission":
		q.Definition = &createInsertPermissionInput{}
	case "delete_permission":
		q.Definition = &createDeletePermissionInput{}
	case "table":
		q.Definition = &trackTableInput{}
	case "function":
		q.Definition = &trackFunctionInput{}
	case "event_trigger":
		q.Definition = &createEventTriggerInput{}
	case "remote_schema":
		q.Definition = &addRemoteSchemaInput{}
	}
	if err := json.Unmarshal(defBody, &q.Definition); err != nil {
		return err
	}
	*i = InconsistentMeatadataObject(q)
	return nil
}

func (i InconsistentMeatadataObject) GetType() string {
	return i.Type
}

func (i InconsistentMeatadataObject) GetName() string {
	switch defType := i.Definition.(type) {
	case *createObjectRelationshipInput:
		return defType.Name
	case *createArrayRelationshipInput:
		return defType.Name
	case *createSelectPermissionInput:
		return fmt.Sprintf("%s-permission", defType.Role)
	case *createUpdatePermissionInput:
		return fmt.Sprintf("%s-permission", defType.Role)
	case *createInsertPermissionInput:
		return fmt.Sprintf("%s-permission", defType.Role)
	case *createDeletePermissionInput:
		return fmt.Sprintf("%s-permission", defType.Role)
	case *trackTableInput:
		return defType.Name
	case *trackFunctionInput:
		return defType.Name
	case *createEventTriggerInput:
		return defType.Name
	case *addRemoteSchemaInput:
		return defType.Name
	}
	return "N/A"
}

func (i InconsistentMeatadataObject) GetDescription() string {
	switch defType := i.Definition.(type) {
	case *createObjectRelationshipInput:
		return fmt.Sprintf("relationship of table %s in %s schema", defType.Table.Name, defType.Table.Schema)
	case *createArrayRelationshipInput:
		return fmt.Sprintf("relationship of table %s in %s schema", defType.Table.Name, defType.Table.Schema)
	case *createSelectPermissionInput:
		return fmt.Sprintf("%s on table %s in %s schema", i.Type, defType.Table.Name, defType.Table.Schema)
	case *createUpdatePermissionInput:
		return fmt.Sprintf("%s on table %s in %s schema", i.Type, defType.Table.Name, defType.Table.Schema)
	case *createInsertPermissionInput:
		return fmt.Sprintf("%s on table %s in %s schema", i.Type, defType.Table.Name, defType.Table.Schema)
	case *createDeletePermissionInput:
		return fmt.Sprintf("%s on table %s in %s schema", i.Type, defType.Table.Name, defType.Table.Schema)
	case *trackTableInput:
		return fmt.Sprintf("table %s in %s schema", defType.tableSchema.Name, defType.tableSchema.Schema)
	case *trackFunctionInput:
		return fmt.Sprintf("function %s in %s schema", defType.Name, defType.Schema)
	case *createEventTriggerInput:
		return fmt.Sprintf("event trigger %s on table %s in %s schema", defType.Name, defType.Table.Name, defType.Table.Schema)
	case *addRemoteSchemaInput:
		url := defType.Definition["url"]
		urlFromEnv, ok := defType.Definition["url_from_env"]
		if ok {
			url = fmt.Sprintf("the url from the value of env var %s", urlFromEnv)
		}
		return fmt.Sprintf("remote schema %s at %s", defType.Name, url)
	}
	return "N/A"
}

func (i InconsistentMeatadataObject) GetReason() string {
	return i.Reason
}

type RunSQLInput struct {
	SQL                      string `json:"sql" yaml:"sql"`
	Source                   string `json:"source,omitempty" yaml:"source,omitempty"`
	Cascade                  bool   `json:"cascade,omitempty" yaml:"cascade,omitempty"`
	ReadOnly                 bool   `json:"read_only,omitempty" yaml:"read_only,omitempty"`
	CheckMetadataConsistency *bool  `json:"check_metadata_consistency,omitempty" yaml:"check_metadata_consistency,omitempty"`
}

type tableConfig struct {
	name, schema string
	transition.Transition
}

type relationshipConfig struct {
	tableName, schemaName, name string
	transition.Transition
}

type permissionConfig struct {
	tableName, schemaName, permType, role string
	transition.Transition
}

type computedFieldConfig struct {
	tableName, schemaName, name string
	transition.Transition
}

type functionConfig struct {
	name, schema string
	transition.Transition
}

type eventTriggerConfig struct {
	name string
	transition.Transition
}

type remoteSchemaConfig struct {
	name string
	transition.Transition
}

type queryCollectionConfig struct {
	name      string
	allowList bool
	transition.Transition
}

type queryInCollectionConfig struct {
	collectionName string
	queryName      string
	transition.Transition
}

type allowListConfig struct {
	collection string
	transition.Transition
}

type remoteRelationshipConfig struct {
	tableName, schemaName, name string
	transition.Transition
}

type cronTriggerConfig struct {
	name string
	transition.Transition
}

type actionConfig struct {
	name string
	transition.Transition
}
type actionPermissionConfig struct {
	action string
	transition.Transition
}
