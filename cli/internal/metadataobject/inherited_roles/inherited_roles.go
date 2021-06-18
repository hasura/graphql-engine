package inheritedroles

import (
	"io/ioutil"
	"path/filepath"

	errors2 "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/errors"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v2"
)

const (
	MetadataFilename string = "inherited_roles.yaml"
)

type InheritedRolesConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *InheritedRolesConfig {
	return &InheritedRolesConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (ir *InheritedRolesConfig) Validate() error {
	return nil
}

func (ir *InheritedRolesConfig) CreateFiles() error {
	// skip creating files by default, since this is an experimental feature
	// which has to be enabled on the server using a flag.
	return nil
}

func (ir *InheritedRolesConfig) Build(metadata *yaml.MapSlice) errors2.ErrParsingMetadataObject {
	data, err := ioutil.ReadFile(filepath.Join(ir.MetadataDir, MetadataFilename))
	if err != nil {
		return ir.Error(err)
	}
	item := yaml.MapItem{
		Key:   "inherited_roles",
		Value: []yaml.MapSlice{},
	}
	err = yaml.Unmarshal(data, &item.Value)
	if err != nil {
		return ir.Error(err)
	}
	*metadata = append(*metadata, item)
	return nil
}

func (ir *InheritedRolesConfig) Export(metadata yaml.MapSlice) (map[string][]byte, errors2.ErrParsingMetadataObject) {
	var inheritedRoles interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "inherited_roles" {
			continue
		}
		inheritedRoles = item.Value
	}
	if inheritedRoles == nil {
		ir.logger.WithFields(logrus.Fields{
			"reason": "not found in metadata",
		}).Debugf("skipped building %s", ir.Name())
		return nil, nil
	}
	data, err := yaml.Marshal(inheritedRoles)
	if err != nil {
		return nil, ir.Error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(ir.MetadataDir, MetadataFilename)): data,
	}, nil
}

func (ir *InheritedRolesConfig) Name() string {
	return "inherited_roles"
}

func (ir *InheritedRolesConfig) Error(err error, additionalContext ...string) errors2.ErrParsingMetadataObject {
	return errors2.NewErrParsingMetadataObject(ir.Name(), MetadataFilename, additionalContext, err)
}
