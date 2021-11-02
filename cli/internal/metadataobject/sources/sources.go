package sources

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"

	"github.com/sirupsen/logrus"

	"github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2"
	goyaml "gopkg.in/yaml.v2"
	v3yaml "gopkg.in/yaml.v3"
)

const (
	sourcesDirectory   string = "databases"
	functionsDirectory string = "functions"
	tablesDirectory    string = "tables"
)

type SourceWithNormalFields struct {
	Name          string      `yaml:"name"`
	Kind          string      `yaml:"kind"`
	Configuration interface{} `yaml:"configuration"`
	QueryTags     interface{} `yaml:"query_tags,omitempty"`
}
type Source struct {
	SourceWithNormalFields `yaml:",inline"`
	Tables                 interface{} `yaml:"tables"`
	Functions              interface{} `yaml:"functions,omitempty"`
}

type SourceConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *SourceConfig {
	return &SourceConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (t *SourceConfig) Validate() error {
	return nil
}

func (t *SourceConfig) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	path := filepath.Join(t.MetadataDir, sourcesDirectory, t.Filename())
	if err := os.MkdirAll(filepath.Dir(path), 0744); err != nil {
		return err
	}
	err = ioutil.WriteFile(path, data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (t *SourceConfig) Build(metadata *goyaml.MapSlice) metadataobject.ErrParsingMetadataObject {
	sourceFile := filepath.Join(t.MetadataDir, sourcesDirectory, t.Filename())
	sourcesBytes, err := ioutil.ReadFile(sourceFile)
	if err != nil {
		return t.error(err)
	}
	// unmarshal everything else except tables and functions
	var sourceNormalFields []SourceWithNormalFields
	if err := v3yaml.Unmarshal(sourcesBytes, &sourceNormalFields); err != nil {
		return t.error(fmt.Errorf("parsing error: %w", err))
	}
	var sources []Source
	for idx, minisource := range sourceNormalFields {
		source := Source{
			SourceWithNormalFields: minisource,
		}

		// get tables node
		tablepath, err := yaml.PathString(fmt.Sprintf("$[%d].tables", idx))
		if err != nil {
			return t.error(err)
		}
		tableNode, err := tablepath.ReadNode(bytes.NewReader(sourcesBytes))
		if err == nil {
			tableNodeBytes, err := ioutil.ReadAll(tableNode)
			if err != nil {
				return t.error(err)
			}
			var tablesKey interface{}
			err = v3yaml.Unmarshal(tableNodeBytes, metadatautil.NewYamlDecoder(
				metadatautil.YamlDecoderOpts{
					IncludeTagBaseDirectory: filepath.Join(t.MetadataDir, sourcesDirectory),
				},
				&tablesKey,
			))
			if err != nil {
				return t.error(err)
			}
			source.Tables = tablesKey
		} else {
			t.logger.Debugf("building metadata: table node not found for %s", source.Name)
		}

		// get functions node
		functionsPath, err := yaml.PathString(fmt.Sprintf("$[%d].functions", idx))
		if err != nil {
			return t.error(err)
		}
		functionsNode, err := functionsPath.ReadNode(bytes.NewReader(sourcesBytes))
		if err == nil {
			functionsNodeBytes, err := ioutil.ReadAll(functionsNode)
			if err != nil {
				return t.error(err)
			}
			var functionsKey interface{}
			err = v3yaml.Unmarshal(functionsNodeBytes, metadatautil.NewYamlDecoder(
				metadatautil.YamlDecoderOpts{
					IncludeTagBaseDirectory: filepath.Join(t.MetadataDir, sourcesDirectory),
				},
				&functionsKey,
			))
			if err != nil {
				return t.error(err)
			}
			source.Functions = functionsKey
		} else {
			t.logger.Debugf("building metadata: functions node not found for %s", source.Name)
		}

		sources = append(sources, source)
	}

	sourcesStructBytes, err := goyaml.Marshal(sources)
	if err != nil {
		return t.error(err)
	}
	var item = goyaml.MapItem{
		Key:   "sources",
		Value: []yaml.MapSlice{},
	}
	if err := v3yaml.Unmarshal(sourcesStructBytes, &item.Value); err != nil {
		return t.error(fmt.Errorf("parsing error: %w \n%v\n", err, string(sourcesStructBytes)))
	}
	*metadata = append(*metadata, item)
	return nil
}

func (t *SourceConfig) Export(metadata goyaml.MapSlice) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	metadataBytes, err := goyaml.Marshal(metadata)
	if err != nil {
		return nil, t.error(err)
	}
	files := map[string][]byte{}
	// Build sources.yaml
	// sources.yaml
	var sources []*Source
	sourcePath, err := yaml.PathString("$.sources")
	if err != nil {
		return nil, t.error(err)
	}
	if err := sourcePath.Read(bytes.NewReader(metadataBytes), &sources); err != nil {
		return nil, t.error(err)
	}
	for idx, source := range sources {
		var tableTags []string
		var functionTags []string

		// populate !include syntax
		var tablesKey []struct {
			Table struct {
				Name string `yaml:"name"`
				// Depending on the datasource the namespacing parameter can change.
				// for example in pg and mssql the namespacing is done with the concept of schemas
				// and in cases like bigquery it'll be called "dataset"
				Schema  *string `yaml:"schema"`
				Dataset *string `yaml:"dataset"`
			} `yaml:"table"`
		}
		path := fmt.Sprintf("$.sources[%d].tables", idx)
		tablesPath, err := yaml.PathString(path)
		if err != nil {
			return nil, t.error(err)
		}
		if err := tablesPath.Read(bytes.NewReader(metadataBytes), &tablesKey); err != nil {
			t.logger.Debug("reading functions node from metadata", err)
		}

		var rawTables []yaml.MapSlice
		if err := tablesPath.Read(bytes.NewReader(metadataBytes), &rawTables); err != nil {
			t.logger.Debug("reading tables node from metadata", err)
		}
		for idx, table := range tablesKey {
			var tableNamespaceIdentifier string
			// according to what namespacing parameter is set w.r.t to the source
			// return the name of the namespacing identifier
			// for pg and mssql it'll be schema name
			// for big query this will be dataset name
			if table.Table.Schema != nil {
				tableNamespaceIdentifier = *table.Table.Schema
			} else if table.Table.Dataset != nil {
				tableNamespaceIdentifier = *table.Table.Dataset
			}

			tableFileName := fmt.Sprintf("%s_%s.yaml", tableNamespaceIdentifier, table.Table.Name)
			tableIncludeTag := fmt.Sprintf("%s %s", "!include", tableFileName)
			tableTags = append(tableTags, tableIncludeTag)

			// build <source>/tables/<table_primary_key>.yaml
			b, err := yaml.Marshal(rawTables[idx])
			if err != nil {
				return nil, t.error(err)
			}
			tableFilePath := filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, tablesDirectory, tableFileName))
			files[tableFilePath] = b
		}
		tableTagsFilePath := filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, tablesDirectory, "tables.yaml"))
		tableTagsBytes, err := yaml.Marshal(tableTags)
		if err != nil {
			return nil, t.error(fmt.Errorf("building contents for %v: %w", tableTagsFilePath, err))
		}
		files[tableTagsFilePath] = tableTagsBytes
		source.Tables = fmt.Sprintf("!include %s", filepath.ToSlash(filepath.Join(source.Name, tablesDirectory, "tables.yaml")))

		var functions []struct {
			Function struct {
				Name   string `yaml:"name"`
				Schema string `yaml:"schema"`
			} `yaml:"function"`
		}
		functionsPath, err := yaml.PathString(fmt.Sprintf("$.sources[%d].functions", idx))
		if err != nil {
			return nil, t.error(err)
		}
		if err := functionsPath.Read(bytes.NewReader(metadataBytes), &functions); err != nil {
			t.logger.Debug("reading functions node from metadata", err)
		}
		var rawFunctions []yaml.MapSlice
		if err := functionsPath.Read(bytes.NewReader(metadataBytes), &rawFunctions); err != nil {
			t.logger.Debug("reading functions node from metadata", err)
		}
		for idx, function := range functions {
			functionFileName := fmt.Sprintf("%s_%s.yaml", function.Function.Schema, function.Function.Name)
			includeTag := fmt.Sprintf("%s %s", "!include", functionFileName)
			functionTags = append(functionTags, includeTag)

			// build <source>/functions/<function_primary_key>.yaml
			b, err := yaml.Marshal(rawFunctions[idx])
			if err != nil {
				return nil, t.error(err)
			}
			functionFilePath := filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, functionsDirectory, functionFileName))
			files[functionFilePath] = b
		}
		if len(functions) > 0 {
			functionsTagsFilePath := filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, functionsDirectory, "functions.yaml"))
			functionTagsBytes, err := yaml.Marshal(functionTags)
			if err != nil {
				return nil, t.error(fmt.Errorf("building contents for %v: %w", functionsTagsFilePath, err))
			}
			files[functionsTagsFilePath] = functionTagsBytes
			source.Functions = filepath.ToSlash(fmt.Sprintf("!include %s", filepath.ToSlash(filepath.Join(source.Name, functionsDirectory, "functions.yaml"))))
		}
	}

	sourcesYamlBytes, err := yaml.Marshal(sources)
	if err != nil {
		return nil, t.error(err)
	}
	files[filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, t.Filename()))] = sourcesYamlBytes
	return files, nil
}

func (t *SourceConfig) Key() string {
	return "sources"
}

func (t *SourceConfig) Filename() string {
	return "databases.yaml"
}

func (t *SourceConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(t, err, additionalContext...)
}
