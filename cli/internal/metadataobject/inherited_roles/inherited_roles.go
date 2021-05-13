package inheritedroles

import (
	"io/ioutil"
	"path/filepath"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli"
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

func (t *InheritedRolesConfig) Validate() error {
	return nil
}

func (t *InheritedRolesConfig) CreateFiles() error {
	// skip creating files by default, since this is an experimental feature
	// which has to be enabled on the server using a flag.
	return nil
}

func (t *InheritedRolesConfig) Build(metadata *yaml.MapSlice) error {
	data, err := ioutil.ReadFile(filepath.Join(t.MetadataDir, MetadataFilename))
	if err != nil {
		return err
	}
	item := yaml.MapItem{
		Key:   "inherited_roles",
		Value: []yaml.MapSlice{},
	}
	err = yaml.Unmarshal(data, &item.Value)
	if err != nil {
		return err
	}
	*metadata = append(*metadata, item)
	return nil
}

func (t *InheritedRolesConfig) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
	var inheritedRoles interface{}
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || k != "inherited_roles" {
			continue
		}
		inheritedRoles = item.Value
	}
	if inheritedRoles == nil {
		t.logger.WithFields(logrus.Fields{
			"reason": "not found in metadata",
		}).Debugf("skipped building %s", t.Name())
		return nil, nil
	}
	data, err := yaml.Marshal(inheritedRoles)
	if err != nil {
		return nil, err
	}
	return map[string][]byte{
		filepath.Join(t.MetadataDir, MetadataFilename): data,
	}, nil
}

func (t *InheritedRolesConfig) Name() string {
	return "inherited_roles"
}
