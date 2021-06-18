package tables

import (
	"path/filepath"

	errors2 "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/errors"

	"github.com/hasura/graphql-engine/cli/v2"

	"gopkg.in/yaml.v2"
)

/*
V3MetadataTableConfig is responsible for exporting and applying "tables" metadata objects
in config v2 format on a server with v3 metadata
*/
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
func (t *V3MetadataTableConfig) Export(md yaml.MapSlice) (map[string][]byte, errors2.ErrParsingMetadataObject) {
	metadataBytes, err := yaml.Marshal(md)
	if err != nil {
		return nil, t.Error(err)
	}
	var metadata struct {
		Sources []struct {
			Name   string          `yaml:"name"'`
			Tables []yaml.MapSlice `yaml:"tables"`
		} `yaml:"sources"`
	}
	var tables interface{}
	if err := yaml.Unmarshal(metadataBytes, &metadata); err != nil {
		return nil, t.Error(err)
	}
	if len(metadata.Sources) > 0 {
		tables = metadata.Sources[0].Tables
	}
	if tables == nil {
		tables = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(tables)
	if err != nil {
		return nil, t.Error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(t.MetadataDir, MetadataFilename)): data,
	}, nil
}
