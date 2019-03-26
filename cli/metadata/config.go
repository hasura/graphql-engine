package metadata

import (
	"encoding/json"
	"fmt"

	"github.com/hasura/graphql-engine/cli/assets"
	"github.com/pkg/errors"
)

const (
	defaultSchema string = "public"
)

// Config is a object storing all the possible information required for track command
type Config struct {
	// Schemas which needs to be tracked
	Schemas []string

	// Tables which needs to be tracked
	// If AllTables is set to true, this will be ignored
	Tables []string

	// AllTables tracks all tables
	AllTables bool

	// AllRelationShips tracks all the possible relationships
	AllRelationShips bool

	// ExportAsMigration generates Down query
	ExportAsMigration bool

	// internal configs
	hasuraDBConfig *hasuraDBConfig
	tablesInfo     []tableConfig
	trackInfo      trackInfo
}

type trackInfo struct {
	tableUp   []hasuraDBQuery
	tableDown []hasuraDBQuery

	relationshipUp   []hasuraDBQuery
	relationshipDown []hasuraDBQuery
}

// NewDefaultConfig returns a config with default values
func NewDefaultConfig(endpoint string, accessSecret string) (*Config, error) {
	config := &Config{
		tablesInfo: make([]tableConfig, 0),
		trackInfo: trackInfo{
			tableUp:          make([]hasuraDBQuery, 0),
			tableDown:        make([]hasuraDBQuery, 0),
			relationshipUp:   make([]hasuraDBQuery, 0),
			relationshipDown: make([]hasuraDBQuery, 0),
		},
	}
	hasuraDBConfig, err := newhasuraDB(endpoint, accessSecret)
	if err != nil {
		return nil, err
	}
	config.hasuraDBConfig = hasuraDBConfig
	return config, nil
}

// AppendSchema adds a new a schema
func (c *Config) AppendSchema(schemaName string) {
	for _, name := range c.Schemas {
		if name == schemaName {
			break
		}
	}
	c.Schemas = append(c.Schemas, schemaName)
}

// AppendTable adds a new table
func (c *Config) AppendTable(tableName string) {
	for _, name := range c.Tables {
		if name == tableName {
			break
		}
	}
	c.Tables = append(c.Tables, tableName)
}

// Scan sends out run_sql query to fetch the required schema information
func (c *Config) Scan() error {
	for _, schema := range c.Schemas {
		data, err := c.fetchSchemaMetadata(schema)
		if err != nil {
			return errors.Wrap(err, "cannot fetch schema metadata")
		}

		columns := data.Result[0]
		var tableNameIndex int
		var tableTrackedIndex int
		var relationshipsIndex int
		for index, column := range columns {
			switch column {
			case "table_name":
				tableNameIndex = index
			case "is_table_tracked":
				tableTrackedIndex = index
			case "relationships":
				relationshipsIndex = index
			}
		}

		for _, tableItem := range data.Result[1:] {
			tableName := tableItem[tableNameIndex]
			tableIsTracked := tableItem[tableTrackedIndex]
			table := newTable(tableName, schema)
			if tableIsTracked == "t" {
				table.SetIsTracked(true)
			} else {
				table.SetIsTracked(false)
			}
			err := json.Unmarshal([]byte(tableItem[relationshipsIndex]), &table.relationShips)
			if err != nil {
				return err
			}

			c.tablesInfo = append(c.tablesInfo, *table)
		}
	}
	return nil
}

// Track sends out a bulk query to track the tables and relationship based on the configuration provided
func (c *Config) Track() error {
	for _, tableItem := range c.tablesInfo {
		if len(c.Tables) != 0 {
			ok := c.checkTableToBeTracked(tableItem.name)
			if !ok {
				continue
			}
		}

		if !tableItem.GetIsTracked() {
			up := tableItem.Track()
			c.trackInfo.tableUp = append(c.trackInfo.tableUp, up)

			down := tableItem.UnTrack()
			c.trackInfo.tableDown = append(c.trackInfo.tableDown, down)
		}

		if c.AllRelationShips {
			for _, relationship := range tableItem.relationShips {
				ok := c.checkSchemaToBeTracked(relationship.RefTableSchema)
				if !ok {
					return fmt.Errorf("can't track relationship: need to add --schema %s", relationship.RefTableSchema)
				}
			}

			up, down := tableItem.TrackRelationShips()
			c.trackInfo.relationshipUp = append(c.trackInfo.relationshipUp, up...)
			c.trackInfo.relationshipDown = append(c.trackInfo.relationshipDown, down...)
		}
	}

	if c.ExportAsMigration {
		return nil
	}

	bulkQuery := newBulkQuery()
	bulkQuery.Args = append(bulkQuery.Args, c.trackInfo.tableUp...)
	bulkQuery.Args = append(bulkQuery.Args, c.trackInfo.relationshipUp...)
	if len(bulkQuery.Args) == 0 {
		return nil
	}

	_, err := c.hasuraDBConfig.sendQuery(bulkQuery)
	if err != nil {
		return err
	}
	return nil
}

// UnTrack sends out a bulk query to untrack the tables and relationship based on the configuration provided
func (c *Config) UnTrack() error {
	for _, tableItem := range c.tablesInfo {
		if len(c.Tables) != 0 {
			ok := c.checkTableToBeTracked(tableItem.name)
			if !ok {
				continue
			}
		}

		if tableItem.GetIsTracked() {
			// Generate UnTrackQuery
			up := tableItem.UnTrack()
			c.trackInfo.tableUp = append(c.trackInfo.tableUp, up)

			down := tableItem.Track()
			c.trackInfo.tableDown = append(c.trackInfo.tableDown, down)
		}

		if c.AllRelationShips {
			for _, relationship := range tableItem.relationShips {
				ok := c.checkSchemaToBeTracked(relationship.RefTableSchema)
				if !ok {
					return fmt.Errorf("can't track relationship: need to add --schema %s", relationship.RefTableSchema)
				}
			}

			up, down := tableItem.UnTrackRelationShips()
			c.trackInfo.relationshipUp = append(c.trackInfo.relationshipUp, up...)
			c.trackInfo.relationshipDown = append(c.trackInfo.relationshipDown, down...)
		}
	}

	if c.ExportAsMigration {
		return nil
	}

	bulkQuery := newBulkQuery()
	bulkQuery.Args = append(bulkQuery.Args, c.trackInfo.relationshipUp...)
	bulkQuery.Args = append(bulkQuery.Args, c.trackInfo.tableUp...)
	if len(bulkQuery.Args) == 0 {
		return nil
	}

	_, err := c.hasuraDBConfig.sendQuery(bulkQuery)
	if err != nil {
		return err
	}
	return nil
}

func (c *Config) fetchSchemaMetadata(schemaName string) (*sqlRes, error) {
	templateString, err := assets.Asset("metadata/sql/schema_info.sql.tmpl")
	if err != nil {
		return nil, err
	}

	query := newSQLQuery(fmt.Sprintf(string(templateString), schemaName))
	schemaByte, err := c.hasuraDBConfig.sendQuery(*query)
	if err != nil {
		return nil, err
	}
	var schemaData sqlRes
	err = json.Unmarshal(schemaByte, &schemaData)
	if err != nil {
		return nil, err
	}

	return &schemaData, nil
}

func (c *Config) checkSchemaToBeTracked(schemaName string) bool {
	for _, schemaItem := range c.Schemas {
		if schemaItem == schemaName {
			return true
		}
	}
	return false
}

func (c *Config) checkTableToBeTracked(tableName string) bool {
	for _, tableItem := range c.Tables {
		if tableItem == tableName {
			return true
		}
	}
	return false
}
