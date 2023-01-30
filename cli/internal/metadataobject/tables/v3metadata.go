package tables

import (
	"bytes"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/hasura/graphql-engine/cli/v2"

	"gopkg.in/yaml.v3"
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
func (t *V3MetadataTableConfig) Export(md map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "tables.V3MetadataTableConfig.Export"
	metadataBytes, err := yaml.Marshal(md)
	if err != nil {
		return nil, errors.E(op, t.error(err))
	}
	var metadata struct {
		Sources []struct {
			Name   string      `yaml:"name"`
			Tables []yaml.Node `yaml:"tables"`
		} `yaml:"sources"`
	}
	var tables interface{}
	if err := yaml.Unmarshal(metadataBytes, &metadata); err != nil {
		return nil, errors.E(op, t.error(err))
	}
	if len(metadata.Sources) > 0 {
		tables = metadata.Sources[0].Tables
	}
	if tables == nil {
		tables = make([]interface{}, 0)
	}
	var buf bytes.Buffer
	err = metadataobject.GetEncoder(&buf).Encode(tables)
	if err != nil {
		return nil, errors.E(op, t.error(err))
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(t.MetadataDir, t.Filename())): buf.Bytes(),
	}, nil
}
