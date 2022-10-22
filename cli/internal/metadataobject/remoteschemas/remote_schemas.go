package remoteschemas

import (
	"bytes"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/sirupsen/logrus"
	"github.com/vektah/gqlparser"
	"github.com/vektah/gqlparser/ast"
	"github.com/vektah/gqlparser/formatter"
	"gopkg.in/yaml.v3"
)

type SchemaDefinition struct {
	Schema string `yaml:"schema"`
}

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
	buf := new(bytes.Buffer)
	err := metadataobject.GetEncoder(buf).Encode(v)
	if err != nil {
		return err
	}

	path := filepath.Join(r.MetadataDir, r.Filename())
	if err := os.MkdirAll(filepath.Dir(path), 0744); err != nil {
		return err
	}
	err = ioutil.WriteFile(path, buf.Bytes(), 0644)
	if err != nil {
		return err
	}
	return nil
}
func (r *RemoteSchemaConfig) Build() (map[string]interface{}, metadataobject.ErrParsingMetadataObject) {
	data, err := ioutil.ReadFile(filepath.Join(r.MetadataDir, r.Filename()))
	if err != nil {
		return nil, r.error(err)
	}
	var obj []yaml.Node
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, r.error(err)
	}
	return map[string]interface{}{r.Key(): obj}, nil
}

type remoteSchema struct {
	Name                yaml.Node    `yaml:"name,omitempty"`
	Defintion           yaml.Node    `yaml:"definition,omitempty"`
	Comment             yaml.Node    `yaml:"comment,omitempty"`
	Permissions         []permission `yaml:"permissions,omitempty"`
	RemoteRelationships yaml.Node    `yaml:"remote_relationships,omitempty"`
}

type permission struct {
	Role       yaml.Node  `yaml:"role,omitempty"`
	Definition definition `yaml:"definition,omitempty"`
}

type definition struct {
	Schema string `yaml:"schema,omitempty"`
}

func (r *RemoteSchemaConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	var value interface{}
	if v, ok := metadata[r.Key()]; !ok {
		value = []yaml.Node{}
	} else {
		remoteSchemas := []remoteSchema{}
		bs, err := yaml.Marshal(v)
		if err != nil {
			return nil, r.error(err)
		}
		if err := yaml.Unmarshal(bs, &remoteSchemas); err != nil {
			return nil, r.error(err)
		}

		for rsIdx := range remoteSchemas {
			for pIdx := range remoteSchemas[rsIdx].Permissions {
				buf := new(bytes.Buffer)
				gqlFormatter := formatter.NewFormatter(buf)
				schema, err := gqlparser.LoadSchema(&ast.Source{
					Input: remoteSchemas[rsIdx].Permissions[pIdx].Definition.Schema,
				})
				if err != nil {
					r.logger.Infof("formatting permission for role %v in remote schema %v failed", remoteSchemas[rsIdx].Permissions[pIdx].Role, remoteSchemas[rsIdx].Name)
					r.logger.Debugf("loading schema failed for role: %v remote schema: %v error: %v", remoteSchemas[rsIdx].Permissions[pIdx].Role, remoteSchemas[rsIdx].Name, err)
					continue
				}
				gqlFormatter.FormatSchema(schema)
				if buf.Len() > 0 {
					remoteSchemas[rsIdx].Permissions[pIdx].Definition.Schema = buf.String()
				}
			}
		}

		value = remoteSchemas
	}

	var buf bytes.Buffer
	err := metadataobject.GetEncoder(&buf).Encode(value)
	if err != nil {
		return nil, r.error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(r.BaseDirectory(), r.Filename())): buf.Bytes(),
	}, nil
}

func (r *RemoteSchemaConfig) GetFiles() ([]string, metadataobject.ErrParsingMetadataObject) {
	rootFile := filepath.Join(r.BaseDirectory(), r.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, r.error(err)
	}
	return files, nil
}

func (r *RemoteSchemaConfig) WriteDiff(opts metadataobject.WriteDiffOpts) metadataobject.ErrParsingMetadataObject {
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: r, WriteDiffOpts: opts})
	if err != nil {
		return r.error(err)
	}
	return nil
}

func (r *RemoteSchemaConfig) BaseDirectory() string {
	return r.MetadataDir
}

func (r *RemoteSchemaConfig) Key() string {
	return metadataobject.RemoteSchemasKey
}

func (r *RemoteSchemaConfig) Filename() string {
	return "remote_schemas.yaml"
}
func (r *RemoteSchemaConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(r, err, additionalContext...)
}
