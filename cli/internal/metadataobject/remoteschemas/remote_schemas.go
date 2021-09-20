package remoteschemas

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"
)

type RemoteSchemaConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *RemoteSchemaConfig {
	return &RemoteSchemaConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (r *RemoteSchemaConfig) Validate() error {
	return nil
}

func (r *RemoteSchemaConfig) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(r.MetadataDir, r.Filename()), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (r *RemoteSchemaConfig) Build(metadata *yaml.MapSlice) metadataobject.ErrParsingMetadataObject {
	data, err := ioutil.ReadFile(filepath.Join(r.MetadataDir, r.Filename()))
	if err != nil {
		return r.error(err)
	}
	item := yaml.MapItem{
		Key: "remote_schemas",
	}
	var obj []yaml.MapSlice
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return r.error(err)
	}
	if len(obj) != 0 {
		item.Value = obj
		*metadata = append(*metadata, item)
	}
	return nil
}

func (r *RemoteSchemaConfig) Export(metadata yaml.MapSlice) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	var remoteSchemas interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "remote_schemas" {
			continue
		}
		remoteSchemas = item.Value
	}
	if remoteSchemas == nil {
		remoteSchemas = make([]interface{}, 0)
	}
	data, err := yaml.Marshal(remoteSchemas)
	if err != nil {
		return nil, r.error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(r.MetadataDir, r.Filename())): data,
	}, nil
}

func (r *RemoteSchemaConfig) Key() string {
	return "remote_schemas"
}

func (r *RemoteSchemaConfig) Filename() string {
	return "remote_schemas.yaml"
}
func (r *RemoteSchemaConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(r, err, additionalContext...)
}
