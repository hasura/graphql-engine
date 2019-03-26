package metadata

import (
	"fmt"
	"strings"

	"github.com/gobuffalo/flect"
)

type tableConfig struct {
	name string

	schemaName string

	isTracked bool

	relationShips []relationship
}

type relationship struct {
	ColumnMapping map[string]string `json:"column_mapping"`

	RefTableSchema string `json:"ref_table_table_schema"`

	RefTableName string `json:"ref_table"`

	// RelName - if nill, then relationship doesn't exists
	RelName *string `json:"rel_name"`

	RelType string `json:"rel_type"`
}

func newTable(name, schemaName string) *tableConfig {
	tableInfo := &tableConfig{
		name:       name,
		schemaName: schemaName,
	}
	return tableInfo
}

func (t *tableConfig) SetIsTracked(isTracked bool) {
	t.isTracked = isTracked
}

func (t *tableConfig) GetIsTracked() bool {
	return t.isTracked
}

func (t *tableConfig) Track() hasuraDBQuery {
	return hasuraDBQuery{
		QueryType: trackTable,
		Args: tableArg{
			Name:   t.name,
			Schema: t.schemaName,
		},
	}
}

func (t *tableConfig) UnTrack() hasuraDBQuery {
	return hasuraDBQuery{
		QueryType: unTrackTable,
		Args: unTrackTableArg{
			Table: tableArg{
				Name:   t.name,
				Schema: t.schemaName,
			},
		},
	}
}

func (t *tableConfig) TrackRelationShips() ([]hasuraDBQuery, []hasuraDBQuery) {
	up := make([]hasuraDBQuery, 0)
	down := make([]hasuraDBQuery, 0)
	existingRelationShips := make([]string, 0)
	for _, relationship := range t.relationShips {
		if relationship.RelName == nil {
			continue
		}

		existingRelationShips = append(existingRelationShips, *relationship.RelName)
	}
	for _, relationship := range t.relationShips {
		if relationship.RelName != nil {
			continue
		}
		relName := relationship.generateName(existingRelationShips)
		relationship.RelName = &relName

		up = append(up, relationship.Track(t.name, t.schemaName))
		down = append(down, relationship.UnTrack(t.name, t.schemaName))
		existingRelationShips = append(existingRelationShips, relName)
	}
	return up, down
}

func (t *tableConfig) UnTrackRelationShips() ([]hasuraDBQuery, []hasuraDBQuery) {
	up := make([]hasuraDBQuery, 0)
	down := make([]hasuraDBQuery, 0)
	for _, relationship := range t.relationShips {
		if relationship.RelName == nil {
			continue
		}
		up = append(up, relationship.UnTrack(t.name, t.schemaName))
		down = append(down, relationship.Track(t.name, t.schemaName))
	}
	return up, down
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
