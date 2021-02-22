package sources

import (
  "bytes"
  "fmt"
  "io/ioutil"
  "os"
  "path/filepath"

  "github.com/hasura/graphql-engine/cli/metadata/tables"

  "github.com/sirupsen/logrus"

  "github.com/goccy/go-yaml"
  "github.com/hasura/graphql-engine/cli"
  goyaml "gopkg.in/yaml.v2"
  v3yaml "gopkg.in/yaml.v3"
)

const (
	fileName           string = "databases.yaml"
	sourcesDirectory   string = "databases"
	functionsDirectory string = "functions"
	tablesDirectory    string = "tables"
)

type SourceWithNormalFields struct {
	Name          string      `yaml:"name"`
	Configuration interface{} `yaml:"configuration"`
}
type Source struct {
	SourceWithNormalFields `yaml:",inline"`
	Tables                 interface{} `yaml:"tables"`
	Functions              interface{} `yaml:"functions"`
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
	path := filepath.Join(t.MetadataDir, sourcesDirectory, fileName)
	if err := os.MkdirAll(filepath.Dir(path), 0744); err != nil {
	  return err
	}
	err = ioutil.WriteFile(path, data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (t *SourceConfig) Build(metadata *goyaml.MapSlice) error {
	sourceFile := filepath.Join(t.MetadataDir, sourcesDirectory, fileName)
	sourcesBytes, err := ioutil.ReadFile(sourceFile)
	if err != nil {
		return err
	}
	// unmarshal everything else except tables and functions
	var sourceNormalFields []SourceWithNormalFields
	if err := yaml.Unmarshal(sourcesBytes, &sourceNormalFields); err != nil {
		return err
	}
	var sources []Source
	for idx, minisource := range sourceNormalFields {
		source := Source{
			SourceWithNormalFields: minisource,
		}

		// get tables node
		tablepath, err := yaml.PathString(fmt.Sprintf("$[%d].tables", idx))
		if err != nil {
			return err
		}
		tableNode, err := tablepath.ReadNode(bytes.NewReader(sourcesBytes))
		if err == nil {
			tableNodeBytes, err := ioutil.ReadAll(tableNode)
			if err != nil {
				return err
			}
			var tablesKey interface{}
			err = v3yaml.Unmarshal(tableNodeBytes, newSourcesYamlDecoder(
				sourcesYamlDecoderOpts{
					IncludeTagBaseDirectory: filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, tablesDirectory),
				},
				&tablesKey,
			))
			if err != nil {
				return err
			}
			source.Tables = tablesKey
		} else {
			t.logger.Debugf("building metadata: table node not found for %s", source.Name)
		}

		// get functions node
		functionsPath, err := yaml.PathString(fmt.Sprintf("$[%d].functions", idx))
		if err != nil {
			return err
		}
		functionsNode, err := functionsPath.ReadNode(bytes.NewReader(sourcesBytes))
		if err == nil {
			functionsNodeBytes, err := ioutil.ReadAll(functionsNode)
			if err != nil {
				return err
			}
			var functionsKey interface{}
			err = v3yaml.Unmarshal(functionsNodeBytes, newSourcesYamlDecoder(
				sourcesYamlDecoderOpts{
					IncludeTagBaseDirectory: filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, functionsDirectory),
				},
				&functionsKey,
			))
			if err != nil {
				return err
			}
			source.Functions = functionsKey
		} else {
			t.logger.Debugf("building metadata: functions node not found for %s", source.Name)
		}

		sources = append(sources, source)
	}

	sourcesStructBytes, err := goyaml.Marshal(sources)
	if err != nil {
		return err
	}
	var item = goyaml.MapItem{
		Key:   "sources",
		Value: []yaml.MapSlice{},
	}
	if err := goyaml.Unmarshal(sourcesStructBytes, &item.Value); err != nil {
		return err
	}
	*metadata = append(*metadata, item)
	return nil
}

func (t *SourceConfig) Export(metadata goyaml.MapSlice) (map[string][]byte, error) {
	metadataBytes, err := goyaml.Marshal(metadata)
	if err != nil {
		return nil, err
	}
	files := map[string][]byte{}
	// Build sources.yaml
	// sources.yaml
	var sources []*Source
	sourcePath, err := yaml.PathString("$.sources")
	if err != nil {
		return nil, err
	}
	if err := sourcePath.Read(bytes.NewReader(metadataBytes), &sources); err != nil {
		return nil, err
	}
	for idx, source := range sources {
		var tableTags []string
		var functionTags []string

		// populate !include syntax
		var tablesKey []struct {
			Table struct {
				Name   string `yaml:"name"`
				Schema string `yaml:"schema"`
			} `yaml:"table"`
		}
		path := fmt.Sprintf("$.sources[%d].tables", idx)
		tablesPath, err := yaml.PathString(path)
		if err != nil {
			return nil, err
		}
		if err := tablesPath.Read(bytes.NewReader(metadataBytes), &tablesKey); err != nil {
			t.logger.Debug("reading functions node from metadata", err)
		}

		var rawTables []interface{}
		if err := tablesPath.Read(bytes.NewReader(metadataBytes), &rawTables); err != nil {
			t.logger.Debug("reading tables node from metadata", err)
		}
		for idx, table := range tablesKey {
			tableFileName := fmt.Sprintf("%s_%s.yaml", table.Table.Schema, table.Table.Name)
			tableIncludeTag := fmt.Sprintf(fmt.Sprintf("%s %s", "!include", tableFileName))
			tableTags = append(tableTags, tableIncludeTag)

			// build <source>/tables/<table_primary_key>.yaml
			b, err := yaml.Marshal(rawTables[idx])
			if err != nil {
				return nil, err
			}
			tableFilePath := filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, tablesDirectory, tableFileName)
			files[tableFilePath] = b
		}

		var functions []struct {
			Function struct {
				Name   string `yaml:"name"`
				Schema string `yaml:"schema"`
			} `yaml:"function"`
		}
		functionsPath, err := yaml.PathString(fmt.Sprintf("$.sources[%d].functions", idx))
		if err != nil {
			return nil, err
		}
		if err := functionsPath.Read(bytes.NewReader(metadataBytes), &functions); err != nil {
			t.logger.Debug("reading functions node from metadata", err)
		}
		var rawFunctions []interface{}
		if err := functionsPath.Read(bytes.NewReader(metadataBytes), &rawFunctions); err != nil {
			t.logger.Debug("reading functions node from metadata", err)
		}
		for idx, function := range functions {
			functionFileName := fmt.Sprintf("%s_%s.yaml", function.Function.Schema, function.Function.Name)
			includeTag := fmt.Sprintf(fmt.Sprintf("%s %s", "!include", functionFileName))
			functionTags = append(functionTags, includeTag)

			// build <source>/functions/<function_primary_key>.yaml
			b, err := yaml.Marshal(rawFunctions[idx])
			if err != nil {
				return nil, err
			}
			functionFilePath := filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, functionsDirectory, functionFileName)
			files[functionFilePath] = b
		}
		source.Tables = tableTags
		source.Functions = functionTags
	}

	sourcesYamlBytes, err := yaml.Marshal(sources)
	if err != nil {
		return nil, err
	}
	files[filepath.Join(t.MetadataDir, sourcesDirectory, fileName)] = sourcesYamlBytes

	// clear old tables.yaml and functions.yaml files if exists
	if f, _ := os.Stat(filepath.Join(t.MetadataDir, tables.MetadataFilename)); f != nil {
		os.Remove(filepath.Join(t.MetadataDir, tables.MetadataFilename))
	}
	if f, _ := os.Stat(filepath.Join(t.MetadataDir, tables.MetadataFilename)); f != nil {
		os.Remove(filepath.Join(t.MetadataDir, tables.MetadataFilename))
	}

	return files, nil
}

func (t *SourceConfig) Name() string {
	return "sources"
}
