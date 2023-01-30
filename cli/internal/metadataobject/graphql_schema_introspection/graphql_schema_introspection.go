package graphqlschemaintrospection

import (
	"bytes"
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v3"
)

type MetadataObject struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *MetadataObject {
	return &MetadataObject{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (o *MetadataObject) Validate() error {
	return nil
}

func (o *MetadataObject) CreateFiles() error {
	var op errors.Op = "graphqlschemaintrospection.MetadataObject.CreateFiles"
	var v interface{}
	data, err := yaml.Marshal(v)
	if err != nil {
		return errors.E(op, err)
	}
	err = ioutil.WriteFile(filepath.Join(o.MetadataDir, o.Filename()), data, 0644)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

type graphQLSchemaIntrospectionObject struct {
	DisabledForRoles []yaml.Node `yaml:"disabled_for_roles"`
}

func (o *MetadataObject) Build() (map[string]interface{}, error) {
	var op errors.Op = "graphqlschemaintrospection.MetadataObject.Build"
	data, err := metadataobject.ReadMetadataFile(filepath.Join(o.MetadataDir, o.Filename()))
	if err != nil {
		return nil, errors.E(op, o.error(err))
	}
	var obj graphQLSchemaIntrospectionObject
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, errors.E(op, o.error(err))
	}

	return map[string]interface{}{o.Key(): obj}, nil
}

// Export implementation here can probably use metadataobject.DefaultExport
// the only thing stopping us from using it is the metadata API behaviour for this object.
// For other objects an empty value ([] or {}) depending on what type of object will
// "reset" it. ie If I wanted to clear all the cron triggers  on the server I could send metadata
// JSON with "cron_triggers": []. Similarly, If I try to "clear" "graphql_schema_introspection" it'll not work
//
// $ hasura metadata apply -o json --dry-run
// {
//  // ....other objects skipped for brevity
//  "graphql_schema_introspection": {}
//}
// $ hasura md apply
// FATA[0001] error applying metadata
//{
//  "path": "$.args.metadata.graphql_schema_introspection",
//  "error": "the key 'disabled_for_roles' was not present",
//  "code": "parse-failed"
//}
func (o *MetadataObject) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "graphqlschemaintrospection.MetadataObject.Export"
	var object graphQLSchemaIntrospectionObject
	if v, ok := metadata[o.Key()]; ok {
		objectbs, err := yaml.Marshal(v)
		if err != nil {
			return nil, errors.E(op, o.error(err))
		}
		if err := yaml.Unmarshal(objectbs, &object); err != nil {
			return nil, errors.E(op, o.error(err))
		}
	}
	var buf bytes.Buffer
	err := metadataobject.GetEncoder(&buf).Encode(object)
	if err != nil {
		return nil, errors.E(op, o.error(err))
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(o.BaseDirectory(), o.Filename())): buf.Bytes(),
	}, nil
}

func (o *MetadataObject) Key() string {
	return metadataobject.GraphQLSchemaIntrospectionKey
}

func (o *MetadataObject) Filename() string {
	return "graphql_schema_introspection.yaml"
}

func (o *MetadataObject) GetFiles() ([]string, error) {
	var op errors.Op = "graphqlschemaintrospection.MetadataObject.GetFiles"
	rootFile := filepath.Join(o.BaseDirectory(), o.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, errors.E(op, o.error(err))
	}
	return files, nil
}

func (o *MetadataObject) WriteDiff(opts metadataobject.WriteDiffOpts) error {
	var op errors.Op = "graphqlschemaintrospection.MetadataObject.WriteDiff"
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: o, WriteDiffOpts: opts})
	if err != nil {
		return errors.E(op, o.error(err))
	}
	return nil
}

func (o *MetadataObject) BaseDirectory() string {
	return o.MetadataDir
}

func (o *MetadataObject) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(o, err, additionalContext...)
}
