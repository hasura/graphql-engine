package metadata

import (
	"fmt"
	"strings"

	"github.com/gobuffalo/flect"
)

type relationship struct {
	ColumnMapping map[string]string `json:"column_mapping"`

	RefTableSchema string `json:"ref_table_table_schema"`

	RefTableName string `json:"ref_table"`

	// RelName - if nill, then relationship doesn't exists
	RelName *string `json:"rel_name"`

	RelType string `json:"rel_type"`
}

func (r *relationship) Track(tableName, schemaName string) hasuraDBQuery {
	query := hasuraDBQuery{}
	switch r.RelType {
	case "object":
		query.QueryType = createObjectRelationShip
		object := objectRelArg{
			Table: tableArg{
				Name:   tableName,
				Schema: schemaName,
			},
			Name: *r.RelName,
		}
		if len(r.ColumnMapping) == 1 {
			// Use foreign_key_constraint_on
			columns := getColumnMappingKeySlice(r.ColumnMapping)
			object.Using.ForeignKeyConstraintOn = columns[0]
		} else {
			// Use manual_configuration
			object.Using.ManualConfiguration = &manualConfiguration{
				RemoteTable: tableArg{
					Name:   r.RefTableName,
					Schema: r.RefTableSchema,
				},
				ColumnMapping: r.ColumnMapping,
			}
		}
		query.Args = object
	case "array":
		query.QueryType = createArrayRelationShip
		array := arrayRelArg{
			Name: *r.RelName,
			Table: tableArg{
				Name:   tableName,
				Schema: schemaName,
			},
		}
		if len(r.ColumnMapping) == 1 {
			// Use foreign_key_constraint_on
			columns := getColumnMappingKeySlice(r.ColumnMapping)
			array.Using.ForeignKeyConstraintOn = &arrayForeignKeyConstraintOn{
				Table: tableArg{
					Name:   r.RefTableName,
					Schema: r.RefTableSchema,
				},
				Column: columns[0],
			}
		} else {
			// Use manual_configuration
			array.Using.ManualConfiguration = &manualConfiguration{
				RemoteTable: tableArg{
					Name:   r.RefTableName,
					Schema: r.RefTableSchema,
				},
				ColumnMapping: r.ColumnMapping,
			}
		}
		query.Args = array
	}
	return query
}

func (r *relationship) UnTrack(tableName, schemaName string) hasuraDBQuery {
	return hasuraDBQuery{
		QueryType: dropRelationShip,
		Args: dropRelationShipArg{
			Table: tableArg{
				Name:   tableName,
				Schema: schemaName,
			},
			Relationship: *r.RelName,
		},
	}
}

func (r *relationship) generateName(names []string) string {
	var relName string
	search := func(name string) bool {
		for _, relItem := range names {
			if relItem == name {
				return true
			}
		}
		return false
	}

	switch r.RelType {
	case "object":
		relName = flect.Singularize(r.RefTableName)
	case "array":
		relName = flect.Pluralize(r.RefTableName)
	}
	if ok := search(relName); !ok {
		return relName
	}

	// Try ByCols
	columns := getColumnMappingKeySlice(r.ColumnMapping)
	return fmt.Sprintf("%s%s", relName, flect.Camelize(fmt.Sprintf("by_%s", strings.Join(columns, "_"))))
}
