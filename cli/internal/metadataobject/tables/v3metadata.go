package tables

import (
	"path/filepath"

	"github.com/hasura/graphql-engine/cli"

	"gopkg.in/yaml.v2"
)

type V3MetadataTableConfig struct {
	*TableConfig
}

func NewV3MetadataTableConfig(ec *cli.ExecutionContext, baseDir string) *V3MetadataTableConfig {
	return &V3MetadataTableConfig{
		&TableConfig{
			MetadataDir: baseDir,
			logger:      ec.Logger,
		},
	}
}
func (t *V3MetadataTableConfig) Export(md yaml.MapSlice) (map[string][]byte, error) {
	metadataBytes, err := yaml.Marshal(md)
	if err != nil {
		return nil, err
	}
	var metadata struct {
		Sources []struct {
			Name   string          `yaml:"name"'`
			Tables []yaml.MapSlice `yaml:"tables"`
		} `yaml:"sources"`
	}
	var tables interface{}
	if err := yaml.Unmarshal(metadataBytes, &metadata); err != nil {
		return nil, err
	}
	if len(metadata.Sources) > 0 {
		tables = metadata.Sources[0].Tables
	}
	if tables == nil {
		tables = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(tables)
	if err != nil {
		return nil, err
	}
	return map[string][]byte{
		filepath.Join(t.MetadataDir, MetadataFilename): data,
	}, nil
}
