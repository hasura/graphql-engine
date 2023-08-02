package functions

import (
	"bytes"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/hasura/graphql-engine/cli/v2"

	"gopkg.in/yaml.v3"
)

type V3MetadataFunctionConfig struct {
	*FunctionConfig
}

func NewV3MetadataFunctionConfig(ec *cli.ExecutionContext, baseDir string) *V3MetadataFunctionConfig {
	return &V3MetadataFunctionConfig{
		&FunctionConfig{
			MetadataDir: baseDir,
			logger:      ec.Logger,
		},
	}
}
func (t *V3MetadataFunctionConfig) Export(md map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "functions.V3MetadataFunctionConfig.Export"
	metadataBytes, err := yaml.Marshal(md)
	if err != nil {
		return nil, errors.E(op, t.error(err))
	}
	var metadata struct {
		Sources []struct {
			Name      string      `yaml:"name"`
			Functions []yaml.Node `yaml:"functions"`
		} `yaml:"sources"`
	}
	var functions []yaml.Node
	if err := yaml.Unmarshal(metadataBytes, &metadata); err != nil {
		return nil, errors.E(op, t.error(err))
	}
	if len(metadata.Sources) > 0 {
		// use functions of first source
		functions = metadata.Sources[0].Functions
	}
	var buf bytes.Buffer
	err = metadataobject.GetEncoder(&buf).Encode(functions)
	if err != nil {
		return nil, errors.E(op, t.error(err))
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(t.MetadataDir, t.Filename())): buf.Bytes(),
	}, nil
}
