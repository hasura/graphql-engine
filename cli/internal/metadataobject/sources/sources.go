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

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v3"
)

const (
	sourcesDirectory   string = "databases"
	functionsDirectory string = "functions"
	tablesDirectory    string = "tables"
)

type SourceWithNormalFields struct {
	Name          string    `yaml:"name"`
	Kind          string    `yaml:"kind"`
	Configuration yaml.Node `yaml:"configuration"`
	QueryTags     yaml.Node `yaml:"query_tags,omitempty"`
	Customization yaml.Node `yaml:"customization,omitempty"`
	HealthCheck	  yaml.Node `yaml:"health_check,omitempty"`
}
type Source struct {
	SourceWithNormalFields `yaml:",inline"`
	Tables                 yaml.Node `yaml:"tables"`
	Functions              yaml.Node `yaml:"functions,omitempty"`
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
	buf := new(bytes.Buffer)
	err := metadataobject.GetEncoder(buf).Encode(v)
	if err != nil {
		return err
	}
	path := filepath.Join(t.MetadataDir, sourcesDirectory, t.Filename())
	if err := os.MkdirAll(filepath.Dir(path), 0744); err != nil {
		return err
	}
	err = ioutil.WriteFile(path, buf.Bytes(), 0644)
	if err != nil {
		return err
	}
	return nil
}

func (t *SourceConfig) Build() (map[string]interface{}, metadataobject.ErrParsingMetadataObject) {
	sourceFile := filepath.Join(t.MetadataDir, sourcesDirectory, t.Filename())
	sourcesBytes, err := metadataobject.ReadMetadataFile(sourceFile)
	if err != nil {
		return nil, t.error(err)
	}
	// unmarshal everything else except tables and functions
	var sourceNormalFields []Source
	if err := yaml.Unmarshal(sourcesBytes, &sourceNormalFields); err != nil {
		return nil, t.error(fmt.Errorf("parsing error: %w", err))
	}
	var sources []Source
	for _, source := range sourceNormalFields {
		if !source.Tables.IsZero() {
			tableNodeBytes, err := yaml.Marshal(source.Tables)
			if err != nil {
				return nil, t.error(err)
			}
			var tablesKey yaml.Node
			err = yaml.Unmarshal(tableNodeBytes, metadatautil.NewYamlDecoder(
				metadatautil.YamlDecoderOpts{
					IncludeTagBaseDirectory: filepath.Join(t.MetadataDir, sourcesDirectory),
				},
				&tablesKey,
			))
			if err != nil {
				return nil, t.error(err)
			}
			source.Tables = tablesKey

		}
		if !source.Functions.IsZero() {
			functionsNodeBytes, err := yaml.Marshal(source.Functions)
			if err != nil {
				return nil, t.error(err)
			}
			var functionsKey yaml.Node
			err = yaml.Unmarshal(functionsNodeBytes, metadatautil.NewYamlDecoder(
				metadatautil.YamlDecoderOpts{
					IncludeTagBaseDirectory: filepath.Join(t.MetadataDir, sourcesDirectory),
				},
				&functionsKey,
			))
			if err != nil {
				return nil, t.error(err)
			}
			source.Functions = functionsKey
		}
		sources = append(sources, source)
	}

	return map[string]interface{}{t.Key(): sources}, nil
}

func (t *SourceConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	var sourcesNode yaml.Node
	var ok bool
	if sourcesNode, ok = metadata[t.Key()]; !ok {
		return nil, nil
	}
	if sourcesNode.IsZero() {
		t.logger.Debugf("skipping exporting %v - not found", t.Key())
		return nil, nil
	}

	sourceBs, err := yaml.Marshal(sourcesNode)
	if err != nil {
		return nil, t.error(err)
	}
	files := map[string][]byte{}
	var sources []Source
	if err := yaml.Unmarshal(sourceBs, &sources); err != nil {
		return nil, t.error(err)
	}

	// Build sources.yaml
	// sources.yaml
	for idx, source := range sources {
		var tableTags []yaml.Node
		var functionTags []yaml.Node
		if !source.Tables.IsZero() {

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
			tablebs, err := yaml.Marshal(source.Tables)
			if err != nil {
				return nil, t.error(err)
			}
			var tableNodes []yaml.Node
			if err := yaml.Unmarshal(tablebs, &tablesKey); err != nil {
				return nil, t.error(err)
			}
			if err := yaml.Unmarshal(tablebs, &tableNodes); err != nil {
				return nil, t.error(err)
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
				tableIncludeTag := yaml.Node{
					Kind:  yaml.ScalarNode,
					Tag:   "!!str",
					Style: yaml.DoubleQuotedStyle,
					Value: fmt.Sprintf("%s %s", "!include", tableFileName),
				}
				tableTags = append(tableTags, tableIncludeTag)

				// build <source>/tables/<table_primary_key>.yaml
				var buf bytes.Buffer
				if err := metadataobject.GetEncoder(&buf).Encode(tableNodes[idx]); err != nil {
					return nil, t.error(err)
				}
				tableFilePath := filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, tablesDirectory, tableFileName))
				files[tableFilePath] = buf.Bytes()
			}
			tableTagsFilePath := filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, tablesDirectory, "tables.yaml"))
			var tableTagsBytes bytes.Buffer
			if err := metadataobject.GetEncoder(&tableTagsBytes).Encode(tableTags); err != nil {
				return nil, t.error(fmt.Errorf("building contents for %v: %w", tableTagsFilePath, err))
			}
			files[tableTagsFilePath] = tableTagsBytes.Bytes()
			sources[idx].Tables = yaml.Node{
				Kind:  yaml.ScalarNode,
				Tag:   "!!str",
				Style: yaml.DoubleQuotedStyle,
				Value: fmt.Sprintf("!include %s", filepath.ToSlash(filepath.Join(source.Name, tablesDirectory, "tables.yaml"))),
			}
		}

		if !source.Functions.IsZero() {
			var functions []struct {
				Function struct {
					Name   string `yaml:"name"`
					Schema string `yaml:"schema"`
				} `yaml:"function"`
			}
			functionsbs, err := yaml.Marshal(source.Functions)
			if err != nil {
				return nil, t.error(err)
			}
			var functionNodes []yaml.Node
			if err := yaml.Unmarshal(functionsbs, &functions); err != nil {
				return nil, t.error(err)
			}
			if err := yaml.Unmarshal(functionsbs, &functionNodes); err != nil {
				return nil, t.error(err)
			}

			for idx, function := range functions {
				functionFileName := fmt.Sprintf("%s_%s.yaml", function.Function.Schema, function.Function.Name)
				includeTag := yaml.Node{
					Kind:  yaml.ScalarNode,
					Tag:   "!!str",
					Style: yaml.DoubleQuotedStyle,
					Value: fmt.Sprintf("%s %s", "!include", functionFileName),
				}
				functionTags = append(functionTags, includeTag)

				// build <source>/functions/<function_primary_key>.yaml
				var buf bytes.Buffer
				if err := metadataobject.GetEncoder(&buf).Encode(functionNodes[idx]); err != nil {
					return nil, t.error(err)
				}
				functionFilePath := filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, functionsDirectory, functionFileName))
				files[functionFilePath] = buf.Bytes()
			}
			if len(functions) > 0 {
				functionsTagsFilePath := filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, functionsDirectory, "functions.yaml"))
				var functionTagsBytes bytes.Buffer
				if err := metadataobject.GetEncoder(&functionTagsBytes).Encode(functionTags); err != nil {
					return nil, t.error(fmt.Errorf("building contents for %v: %w", functionsTagsFilePath, err))
				}
				files[functionsTagsFilePath] = functionTagsBytes.Bytes()
				sources[idx].Functions = yaml.Node{
					Kind:  yaml.ScalarNode,
					Tag:   "!!str",
					Style: yaml.DoubleQuotedStyle,
					Value: filepath.ToSlash(fmt.Sprintf("!include %s", filepath.ToSlash(filepath.Join(source.Name, functionsDirectory, "functions.yaml")))),
				}
			}
		}
	}
	var buf bytes.Buffer
	if err := metadataobject.GetEncoder(&buf).Encode(sources); err != nil {
		return nil, t.error(err)
	}
	files[filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, t.Filename()))] = buf.Bytes()
	return files, nil
}

func (t *SourceConfig) GetFiles() ([]string, metadataobject.ErrParsingMetadataObject) {
	rootFile := filepath.Join(t.MetadataDir, sourcesDirectory, t.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, t.error(err)
	}
	return files, nil
}

func (t *SourceConfig) Key() string {
	return metadataobject.SourcesKey
}

func (t *SourceConfig) Filename() string {
	return "databases.yaml"
}

func (t *SourceConfig) BaseDirectory() string {
	return t.MetadataDir
}

func (t *SourceConfig) WriteDiff(opts metadataobject.WriteDiffOpts) metadataobject.ErrParsingMetadataObject {
	excludePatterns := []string{"tables/tables.yaml", "functions/functions.yaml"}
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: t, WriteDiffOpts: opts, ExcludeFilesPatterns: excludePatterns})
	if err != nil {
		return t.error(err)
	}
	return nil
}

func (t *SourceConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(t, err, additionalContext...)
}
