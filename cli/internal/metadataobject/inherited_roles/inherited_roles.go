package inheritedroles

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v2"
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

func (ir *InheritedRolesConfig) Build(metadata *yaml.MapSlice) metadataobject.ErrParsingMetadataObject {
	data, err := ioutil.ReadFile(filepath.Join(ir.MetadataDir, ir.Filename()))
	if err != nil {
		return ir.error(err)
	}
	item := yaml.MapItem{
		Key:   "inherited_roles",
		Value: []yaml.MapSlice{},
	}
	err = yaml.Unmarshal(data, &item.Value)
	if err != nil {
		return ir.error(err)
	}
	*metadata = append(*metadata, item)
	return nil
}

func (ir *InheritedRolesConfig) Export(metadata yaml.MapSlice) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
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
		}).Debugf("skipped building %s", ir.Key())
		return nil, nil
	}
	data, err := yaml.Marshal(inheritedRoles)
	if err != nil {
		return nil, ir.error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(ir.MetadataDir, ir.Filename())): data,
	}, nil
}

func (ir *InheritedRolesConfig) Key() string {
	return "inherited_roles"
}

func (ir *InheritedRolesConfig) Filename() string {
	return "inherited_roles.yaml"
}

func (ir *InheritedRolesConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(ir, err, additionalContext...)
}
