package sources

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
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
	Name             string    `yaml:"name"`
	Kind             string    `yaml:"kind"`
	Configuration    yaml.Node `yaml:"configuration"`
	QueryTags        yaml.Node `yaml:"query_tags,omitempty"`
	Customization    yaml.Node `yaml:"customization,omitempty"`
	HealthCheck      yaml.Node `yaml:"health_check,omitempty"`
	LogicalModels    yaml.Node `yaml:"logical_models,omitempty"`
	NativeQueries    yaml.Node `yaml:"native_queries,omitempty"`
	StoredProcedures yaml.Node `yaml:"stored_procedures,omitempty"`
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
	var op errors.Op = "sources.SourceConfig.CreateFiles"
	v := make([]interface{}, 0)
	buf := new(bytes.Buffer)
	err := metadataobject.GetEncoder(buf).Encode(v)
	if err != nil {
		return errors.E(op, err)
	}
	path := filepath.Join(t.MetadataDir, sourcesDirectory, t.Filename())
	if err := os.MkdirAll(filepath.Dir(path), 0744); err != nil {
		return errors.E(op, err)
	}
	err = ioutil.WriteFile(path, buf.Bytes(), 0644)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (t *SourceConfig) Build() (map[string]interface{}, error) {
	var op errors.Op = "sources.SourceConfig.Build"
	sourceFile := filepath.Join(t.MetadataDir, sourcesDirectory, t.Filename())
	sourcesBytes, err := metadataobject.ReadMetadataFile(sourceFile)
	if err != nil {
		return nil, errors.E(op, t.error(err))
	}
	// unmarshal everything else except tables and functions
	var sourceNormalFields []Source
	if err := yaml.Unmarshal(sourcesBytes, &sourceNormalFields); err != nil {
		return nil, errors.E(op, t.error(fmt.Errorf("parsing error: %w", err)))
	}
	var sources []Source
	for _, source := range sourceNormalFields {
		if !source.Tables.IsZero() {
			tableNodeBytes, err := yaml.Marshal(source.Tables)
			if err != nil {
				return nil, errors.E(op, t.error(err))
			}
			var tablesKey yaml.Node
			err = yaml.Unmarshal(tableNodeBytes, metadatautil.NewYamlDecoder(
				metadatautil.YamlDecoderOpts{
					IncludeTagBaseDirectory: filepath.Join(t.MetadataDir, sourcesDirectory),
				},
				&tablesKey,
			))
			if err != nil {
				return nil, errors.E(op, t.error(err))
			}
			source.Tables = tablesKey

		}
		if !source.Functions.IsZero() {
			functionsNodeBytes, err := yaml.Marshal(source.Functions)
			if err != nil {
				return nil, errors.E(op, t.error(err))
			}
			var functionsKey yaml.Node
			err = yaml.Unmarshal(functionsNodeBytes, metadatautil.NewYamlDecoder(
				metadatautil.YamlDecoderOpts{
					IncludeTagBaseDirectory: filepath.Join(t.MetadataDir, sourcesDirectory),
				},
				&functionsKey,
			))
			if err != nil {
				return nil, errors.E(op, t.error(err))
			}
			source.Functions = functionsKey
		}
		sources = append(sources, source)
	}

	return map[string]interface{}{t.Key(): sources}, nil
}

func (t *SourceConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "sources.SourceConfig.Export"
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
		return nil, errors.E(op, t.error(err))
	}
	files := map[string][]byte{}
	var sources []Source
	if err := yaml.Unmarshal(sourceBs, &sources); err != nil {
		return nil, errors.E(op, t.error(err))
	}

	// Build sources.yaml
	// sources.yaml
	for idx, source := range sources {
		var tableTags []yaml.Node
		var functionTags []yaml.Node
		if !source.Tables.IsZero() {

			// populate !include syntax
			var tablesKey []struct {
				Table yaml.Node `yaml:"table"`
			}
			tablebs, err := yaml.Marshal(source.Tables)
			if err != nil {
				return nil, errors.E(op, t.error(err))
			}
			var tableNodes []yaml.Node
			if err := yaml.Unmarshal(tablebs, &tablesKey); err != nil {
				return nil, errors.E(op, t.error(err))
			}
			if err := yaml.Unmarshal(tablebs, &tableNodes); err != nil {
				return nil, errors.E(op, t.error(err))
			}
			for idx, table := range tablesKey {
				tableFileName, err := getTableYamlFileName(table.Table)
				if err != nil {
					return nil, errors.E(op, t.error(err))
				}
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
					return nil, errors.E(op, t.error(err))
				}
				tableFilePath := filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, tablesDirectory, tableFileName))
				files[tableFilePath] = buf.Bytes()
			}
			tableTagsFilePath := filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, tablesDirectory, "tables.yaml"))
			var tableTagsBytes bytes.Buffer
			if err := metadataobject.GetEncoder(&tableTagsBytes).Encode(tableTags); err != nil {
				return nil, errors.E(op, t.error(fmt.Errorf("building contents for %v: %w", tableTagsFilePath, err)))
			}
			files[tableTagsFilePath] = tableTagsBytes.Bytes()
			sources[idx].Tables = yaml.Node{
				Kind:  yaml.ScalarNode,
				Tag:   "!!str",
				Style: yaml.DoubleQuotedStyle,
				Value: fmt.Sprintf("!include %s", filepath.ToSlash(filepath.Join(source.Name, tablesDirectory, "tables.yaml"))),
			}
		}
		// Functions are 2 types: PG and Snowflake, which differs in how naming is provided.
		// See https://hasura.io/docs/latest/api-reference/metadata-api/custom-functions/
		// PG:
		// { "function": {"schema": "public", "name": "reset_widget"}, "configuration": ...}
		// Snowflake:
		// { "function": ["search_articles"], "configuration": ...}

		if !source.Functions.IsZero() {
			functionsbs, err := yaml.Marshal(source.Functions)
			if err != nil {
				return nil, errors.E(op, t.error(err))
			}

			var functions []struct {
				Function yaml.Node `yaml:"function"`
			}
			if err := yaml.Unmarshal(functionsbs, &functions); err != nil {
				return nil, errors.E(op, t.error(err))
			}

			var functionNodes []yaml.Node
			if err := yaml.Unmarshal(functionsbs, &functionNodes); err != nil {
				return nil, errors.E(op, t.error(err))
			}

			for idx, functionNode := range functions {
				var functionFileName string
				functionbs, err := yaml.Marshal(functionNode.Function)
				if err != nil {
					return nil, errors.E(op, t.error(err))
				}
				if functionNode.Function.Kind == yaml.MappingNode { // Native DB functions
					var function struct {
						Name   string `yaml:"name"`
						Schema string `yaml:"schema"`
					}
					if err := yaml.Unmarshal(functionbs, &function); err != nil {
						return nil, errors.E(op, t.error(err))
					}
					functionFileName = fmt.Sprintf("%s_%s.yaml", function.Schema, function.Name)
				} else { // Data Connector functions
					var function []string
					if err := yaml.Unmarshal(functionbs, &function); err != nil {
						return nil, errors.E(op, t.error(err))
					}
					functionFileName = fmt.Sprintf("%s.yaml", strings.Join(function[:], "_"))
				}

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
					return nil, errors.E(op, t.error(err))
				}
				functionFilePath := filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, functionsDirectory, functionFileName))
				files[functionFilePath] = buf.Bytes()
			}
			if len(functions) > 0 {
				functionsTagsFilePath := filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, source.Name, functionsDirectory, "functions.yaml"))
				var functionTagsBytes bytes.Buffer
				if err := metadataobject.GetEncoder(&functionTagsBytes).Encode(functionTags); err != nil {
					return nil, errors.E(op, t.error(fmt.Errorf("building contents for %v: %w", functionsTagsFilePath, err)))
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
		return nil, errors.E(op, t.error(err))
	}
	files[filepath.ToSlash(filepath.Join(t.MetadataDir, sourcesDirectory, t.Filename()))] = buf.Bytes()
	return files, nil
}

func (t *SourceConfig) GetFiles() ([]string, error) {
	var op errors.Op = "sources.SourceConfig.GetFiles"
	rootFile := filepath.Join(t.MetadataDir, sourcesDirectory, t.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, errors.E(op, t.error(err))
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

func (t *SourceConfig) WriteDiff(opts metadataobject.WriteDiffOpts) error {
	var op errors.Op = "sources.SourceConfig.WriteDiff"
	excludePatterns := []string{"tables/tables.yaml", "functions/functions.yaml"}
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: t, WriteDiffOpts: opts, ExcludeFilesPatterns: excludePatterns})
	if err != nil {
		return errors.E(op, t.error(err))
	}
	return nil
}

func (t *SourceConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(t, err, additionalContext...)
}
