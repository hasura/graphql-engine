package hasuradb

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/hasura/graphql-engine/cli/migrate/database"

	"github.com/qor/transition"
	log "github.com/sirupsen/logrus"
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
	Type requestTypes `json:"type" yaml:"type"`
	Args interface{}  `json:"args" yaml:"args"`
}

type newHasuraIntefaceQuery struct {
	Type requestTypes `json:"type" yaml:"type"`
	Args interface{}  `json:"args" yaml:"args"`
}

func (h *newHasuraIntefaceQuery) UnmarshalJSON(b []byte) error {
	var q struct {
		Type requestTypes `json:"type"`
		Args interface{}  `json:"args"`
	}
	if err := json.Unmarshal(b, &q); err != nil {
		return err
	}
	h.Type = q.Type
	argBody, err := json.Marshal(q.Args)
	if err != nil {
		return err
	}
	switch h.Type {
	case trackTable, addExistingTableOrView:
		h.Args = &trackTableInput{}
	case untrackTable:
		h.Args = &unTrackTableInput{}
	case createObjectRelationship:
		h.Args = &createObjectRelationshipInput{}
	case createArrayRelationship:
		h.Args = &createArrayRelationshipInput{}
	case setRelationshipComment:
		h.Args = &setRelationshipCommentInput{}
	case dropRelationship:
		h.Args = &dropRelationshipInput{}
	case createInsertPermission:
		h.Args = &createInsertPermissionInput{}
	case dropInsertPermission:
		h.Args = &dropInsertPermissionInput{}
	case createSelectPermission:
		h.Args = &createSelectPermissionInput{}
	case dropSelectPermission:
		h.Args = &dropSelectPermissionInput{}
	case createUpdatePermission:
		h.Args = &createUpdatePermissionInput{}
	case dropUpdatePermission:
		h.Args = &dropUpdatePermissionInput{}
	case createDeletePermission:
		h.Args = &createDeletePermissionInput{}
	case dropDeletePermission:
		h.Args = &dropDeletePermissionInput{}
	case trackFunction:
		h.Args = &trackFunctionInput{}
	case unTrackFunction:
		h.Args = &unTrackFunctionInput{}
	case createEventTrigger:
		h.Args = &createEventTriggerInput{}
	case deleteEventTrigger:
		h.Args = &deleteEventTriggerInput{}
	case addRemoteSchema:
		h.Args = &addRemoteSchemaInput{}
	case removeRemoteSchema:
		h.Args = &removeRemoteSchemaInput{}
	case createQueryCollection:
		h.Args = &createQueryCollectionInput{}
	case dropQueryCollection:
		h.Args = &dropQueryCollectionInput{}
	case addQueryToCollection:
		h.Args = &addQueryToCollectionInput{}
	case dropQueryFromCollection:
		h.Args = &dropQueryFromCollectionInput{}
	case addCollectionToAllowList:
		h.Args = &addCollectionToAllowListInput{}
	case dropCollectionFromAllowList:
		h.Args = &dropCollectionFromAllowListInput{}
	case replaceMetadata:
		h.Args = &replaceMetadataInput{}
	case clearMetadata:
		h.Args = &clearMetadataInput{}
	case runSQL:
		h.Args = &runSQLInput{}
	case addComputedField:
		h.Args = &addComputedFieldInput{}
	case dropComputedField:
		h.Args = &dropComputedFieldInput{}
	default:
		return fmt.Errorf("cannot squash type %s", h.Type)
	}
	if err := json.Unmarshal(argBody, &h.Args); err != nil {
		return err
	}
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
	Path           string            `json:"path"`
	ErrorMessage   string            `json:"error"`
	Internal       *SQLInternalError `json:"internal,omitempty"`
	Message        string            `json:"message,omitempty"`
	Code           string            `json:"code"`
}

type SQLInternalError struct {
	Arguments []string      `json:"arguments"`
	Error     PostgresError `json:"error"`
	Prepared  bool          `json:"prepared"`
	Statement string        `json:"statement"`
}
type PostgresError struct {
	StatusCode  string `json:"status_code"`
	ExecStatus  string `json:"exec_status"`
	Message     string `json:"message"`
	Description string `json:"description"`
	Hint        string `json:"hint"`
}

type SchemaDump struct {
	Opts        []string `json:"opts"`
	CleanOutput bool     `json:"clean_output"`
}

func (h *HasuraError) CMDError() error {
	var errorStrings []string
	errorStrings = append(errorStrings, fmt.Sprintf("[%s] %s (%s)", h.Code, h.ErrorMessage, h.Path))
	if h.migrationFile != "" {
		errorStrings = append(errorStrings, fmt.Sprintf("File: '%s'", h.migrationFile))
	}
	if h.migrationQuery != "" {
		errorStrings = append(errorStrings, fmt.Sprintf("%s", h.migrationQuery))
	}
	if h.Internal != nil {
		// postgres error
		errorStrings = append(errorStrings, fmt.Sprintf("[%s] %s: %s", h.Internal.Error.StatusCode, h.Internal.Error.ExecStatus, h.Internal.Error.Message))
		if len(h.Internal.Error.Description) > 0 {
			errorStrings = append(errorStrings, fmt.Sprintf("Description: %s", h.Internal.Error.Description))
		}
		if len(h.Internal.Error.Hint) > 0 {
			errorStrings = append(errorStrings, fmt.Sprintf("Hint: %s", h.Internal.Error.Hint))
		}
	}
	return fmt.Errorf(strings.Join(errorStrings, "\r\n"))
}

func (h *HasuraError) APIError() error {
	data, err := json.Marshal(&h)
	if err != nil {
		return err
	}
	return fmt.Errorf("Data Error: %s", string(data))
}

func (h *HasuraError) Error(isCMD bool) error {
	var err error
	switch isCMD {
	case true:
		err = h.CMDError()
	case false:
		err = h.APIError()
	}
	log.Debug(err)
	return err
}

type HasuraSQLRes struct {
	ResultType string     `json:"result_type"`
	Result     [][]string `json:"result"`
}

type requestTypes string

const (
	trackTable                  requestTypes = "track_table"
	addExistingTableOrView                   = "add_existing_table_or_view"
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
	runSQL                                   = "run_sql"
	bulkQuery                                = "bulk"
	addComputedField                         = "add_computed_field"
	dropComputedField                        = "drop_computed_field"
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
	Table  tableSchema `json:"table" yaml:"table"`
	IsEnum bool        `json:"is_enum" yaml:"is_enum"`
}

func (t trackTableInput) MarshalJSON() ([]byte, error) {
	return json.Marshal(&struct {
		Name   string `json:"name" yaml:"name"`
		Schema string `json:"schema" yaml:"schema"`
		IsEnum bool   `json:"is_enum" yaml:"is_enum"`
	}{
		Name:   t.Table.Name,
		Schema: t.Table.Schema,
		IsEnum: t.IsEnum,
	})
}

func (t *trackTableInput) UnmarshalJSON(b []byte) error {
	var ts struct {
		Name   string
		Schema string
		IsEnum bool `json:"is_enum"`
	}
	if err := json.Unmarshal(b, &ts); err != nil {
		return err
	}
	t.IsEnum = ts.IsEnum
	if ts.Name == "" {
		var ts struct {
			Table tableSchema
		}
		if err := json.Unmarshal(b, &ts); err != nil {
			return err
		}
		t.Table = ts.Table
		return nil
	}
	if ts.Schema == "" {
		ts.Schema = "public"
	}
	t.Table = tableSchema{
		Name:   ts.Name,
		Schema: ts.Schema,
	}
	return nil
}

type unTrackTableInput struct {
	Table tableSchema `json:"table" yaml:"table"`
}

func (t unTrackTableInput) MarshalJSON() ([]byte, error) {
	return json.Marshal(&struct {
		Name   string `json:"name" yaml:"name"`
		Schema string `json:"schema" yaml:"schema"`
	}{
		Name:   t.Table.Name,
		Schema: t.Table.Schema,
	})
}

func (t *unTrackTableInput) UnmarshalJSON(b []byte) error {
	var ts struct {
		Name   string
		Schema string
	}
	if err := json.Unmarshal(b, &ts); err != nil {
		return err
	}
	if ts.Name == "" {
		var ts struct {
			Table tableSchema
		}
		if err := json.Unmarshal(b, &ts); err != nil {
			return err
		}
		t.Table = ts.Table
		return nil
	}
	if ts.Schema == "" {
		ts.Schema = "public"
	}
	t.Table = tableSchema{
		Name:   ts.Name,
		Schema: ts.Schema,
	}
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
	Comment string      `json:"comment" yaml:"comment"`
}

type dropRelationshipInput struct {
	RelationShip string      `json:"relationship" yaml:"relationship"`
	Table        tableSchema `json:"table" yaml:"table"`
}

type createInsertPermissionInput struct {
	Table      tableSchema `json:"table" yaml:"table"`
	Role       string      `json:"role" yaml:"role"`
	Permission interface{} `json:"permission" yaml:"permission"`
}

type dropInsertPermissionInput struct {
	Table tableSchema `json:"table" yaml:"table"`
	Role  string      `json:"role" yaml:"role"`
}

type createSelectPermissionInput struct {
	Table      tableSchema `json:"table" yaml:"table"`
	Role       string      `json:"role" yaml:"role"`
	Permission interface{} `json:"permission" yaml:"permission"`
}

type dropSelectPermissionInput struct {
	Table tableSchema `json:"table" yaml:"table"`
	Role  string      `json:"role" yaml:"role"`
}

type createUpdatePermissionInput struct {
	Table      tableSchema `json:"table" yaml:"table"`
	Role       string      `json:"role" yaml:"role"`
	Permission interface{} `json:"permission" yaml:"permission"`
}

type dropUpdatePermissionInput struct {
	Table tableSchema `json:"table" yaml:"table"`
	Role  string      `json:"role" yaml:"role"`
}

type createDeletePermissionInput struct {
	Table      tableSchema `json:"table" yaml:"table"`
	Role       string      `json:"role" yaml:"role"`
	Permission interface{} `json:"permission" yaml:"permission"`
}

type dropDeletePermissionInput struct {
	Table tableSchema `json:"table" yaml:"table"`
	Role  string      `json:"role" yaml:"role"`
}

type setPermissionCommentInput struct {
	Table   tableSchema `json:"table" yaml:"table"`
	Role    string      `json:"role" yaml:"role"`
	Type    string      `json:"type" yaml:"type"`
	Comment string      `json:"comment" yaml:"comment"`
}

type createEventTriggerInput struct {
	Name           string                            `json:"name" yaml:"name"`
	Table          tableSchema                       `json:"table" yaml:"table"`
	Webhook        string                            `json:"webhook,omitempty" yaml:"webhook,omitempty"`
	WebhookFromEnv string                            `json:"webhook_from_env,omitempty" yaml:"webhook_from_env,omitempty"`
	Definition     *createEventTriggerOperationInput `json:"definition,omitempty" yaml:"definition,omitempty"`
	Headers        interface{}                       `json:"headers" yaml:"headers"`
	Replace        bool                              `json:"replace" yaml:"replace"`

	createEventTriggerOperationInput
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
		Name           string      `json:"name" yaml:"name"`
		Table          tableSchema `json:"table" yaml:"table"`
		Webhook        string      `json:"webhook,omitempty" yaml:"webhook,omitempty"`
		WebhookFromEnv string      `json:"webhook_from_env,omitempty" yaml:"webhook_from_env,omitempty"`
		Headers        interface{} `json:"headers" yaml:"headers"`
		Replace        bool        `json:"replace" yaml:"replace"`
		Insert         interface{} `json:"insert,omitempty" yaml:"insert,omitempty"`
		Update         interface{} `json:"update,omitempty" yaml:"update,omitempty"`
		Delete         interface{} `json:"delete,omitempty" yaml:"delete,omitempty"`
	}{
		Name:           c.Name,
		Table:          c.Table,
		Webhook:        c.Webhook,
		WebhookFromEnv: c.WebhookFromEnv,
		Headers:        c.Headers,
		Replace:        c.Replace,
		Insert:         c.Insert,
		Update:         c.Update,
		Delete:         c.Delete,
	})
}

type deleteEventTriggerInput struct {
	Name string `json:"name" yaml:"name"`
}

type addRemoteSchemaInput struct {
	Name       string      `json:"name" yaml:"name"`
	Definition interface{} `json:"definition" yaml:"definition"`
	Comment    string      `json:"comment" yaml:"comment"`
}

type removeRemoteSchemaInput struct {
	Name string `json:"name" yaml:"name"`
}

type createQueryCollectionInput struct {
	Name       string      `json:"name" yaml:"name"`
	Comment    string      `json:"comment" yaml:"comment"`
	Definition interface{} `json:"definition" yaml:"definition"`
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
	} `json:"tables" yaml:"tables"`
	Functions        []*trackFunctionInput            `json:"functions" yaml:"functions"`
	QueryCollections []*createQueryCollectionInput    `json:"query_collections" yaml:"query_collections"`
	AllowList        []*addCollectionToAllowListInput `json:"allowlist" yaml:"allowlist"`
	RemoteSchemas    []*addRemoteSchemaInput          `json:"remote_schemas" yaml:"remote_schemas"`
}

func (rmi *replaceMetadataInput) convertToMetadataActions(l *database.CustomList) {
	// track tables
	for _, table := range rmi.Tables {
		t := &trackTableInput{
			Table: tableSchema{
				Name:   table.Table.Name,
				Schema: table.Table.Schema,
			},
		}

		l.PushBack(t)
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
}

type runSQLInput struct {
	SQL string `json:"sql" yaml:"sql"`
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
