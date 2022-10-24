package querycollections

import (
	"bytes"
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v3"

	"github.com/vektah/gqlparser/v2/ast"
	"github.com/vektah/gqlparser/v2/formatter"
	"github.com/vektah/gqlparser/v2/parser"
)

type QueryCollectionConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *QueryCollectionConfig {
	return &QueryCollectionConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (q *QueryCollectionConfig) Validate() error {
	return nil
}

func (q *QueryCollectionConfig) CreateFiles() error {
	var op errors.Op = "querycollections.QueryCollectionsConfig.CreateFiles"
	v := make([]interface{}, 0)
	buf := new(bytes.Buffer)
	err := metadataobject.GetEncoder(buf).Encode(v)
	if err != nil {
		return errors.E(op, err)
	}
	err = ioutil.WriteFile(filepath.Join(q.MetadataDir, q.Filename()), buf.Bytes(), 0644)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (q *QueryCollectionConfig) Build() (map[string]interface{}, error) {
	var op errors.Op = "querycollections.QueryCollectionConfig.Build"
	data, err := metadataobject.ReadMetadataFile(filepath.Join(q.MetadataDir, q.Filename()))
	if err != nil {
		return nil, errors.E(op, q.error(err))
	}
	var obj []yaml.Node
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, errors.E(op, errors.KindBadInput, q.error(err))
	}
	return map[string]interface{}{q.Key(): obj}, nil
}

type querycollection struct {
	Name       yaml.Node  `yaml:"name,omitempty"`
	Definition definition `yaml:"definition"`
}

type definition struct {
	Queries []query `yaml:"queries"`
}
type query struct {
	Name  yaml.Node `yaml:"name,omitempty"`
	Query string    `yaml:"query,omitempty"`
}

func (q *QueryCollectionConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "querycollections.QueryCollectionConfig.Export"
	var value interface{}
	if v, ok := metadata[q.Key()]; !ok {
		value = []yaml.Node{}
	} else {
		var collections []querycollection
		bs, err := yaml.Marshal(v)
		if err != nil {
			return nil, errors.E(op, q.error(err))
		}
		if err := yaml.Unmarshal(bs, &collections); err != nil {
			return nil, errors.E(op, q.error(err))
		}
		for collectionIdx := range collections {
			for queryIdx := range collections[collectionIdx].Definition.Queries {
				buf := new(bytes.Buffer)
				queryDoc, err := parser.ParseQuery(&ast.Source{
					Input: collections[collectionIdx].Definition.Queries[queryIdx].Query,
				})
				if err != nil {
					return nil, errors.E(op, q.error(err))
				}
				gqlFormatter := formatter.NewFormatter(buf, formatter.WithIndent("  "))
				gqlFormatter.FormatQueryDocument(queryDoc)
				if buf.Len() > 0 {
					collections[collectionIdx].Definition.Queries[queryIdx].Query = buf.String()
				}
			}
		}
		value = collections
	}

	var buf bytes.Buffer
	err := metadataobject.GetEncoder(&buf).Encode(value)
	if err != nil {
		return nil, errors.E(op, q.error(err))
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(q.BaseDirectory(), q.Filename())): buf.Bytes(),
	}, nil
}

func (q *QueryCollectionConfig) Key() string {
	return metadataobject.QueryCollectionsKey
}

func (q *QueryCollectionConfig) Filename() string {
	return "query_collections.yaml"
}

func (q *QueryCollectionConfig) GetFiles() ([]string, error) {
	var op errors.Op = "querycollections.QueryCollectionConfig.GetFiles"
	rootFile := filepath.Join(q.BaseDirectory(), q.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, errors.E(op, q.error(err))
	}
	return files, nil
}

func (q *QueryCollectionConfig) WriteDiff(opts metadataobject.WriteDiffOpts) error {
	var op errors.Op = "querycollections.QueryCollectionConfig.WriteDiff"
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: q, WriteDiffOpts: opts})
	if err != nil {
		return errors.E(op, q.error(err))
	}
	return nil
}

func (q *QueryCollectionConfig) BaseDirectory() string {
	return q.MetadataDir
}

func (q *QueryCollectionConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(q, err, additionalContext...)
}
