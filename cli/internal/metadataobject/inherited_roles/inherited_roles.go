package inheritedroles

import (
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v3"
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

func (ir *InheritedRolesConfig) Build() (map[string]interface{}, metadataobject.ErrParsingMetadataObject) {
	data, err := metadataobject.ReadMetadataFile(filepath.Join(ir.MetadataDir, ir.Filename()))
	if err != nil {
		return nil, ir.error(err)
	}
	var obj []yaml.Node
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, ir.error(err)
	}
	return map[string]interface{}{ir.Key(): obj}, nil
}

func (ir *InheritedRolesConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	return metadataobject.DefaultExport(ir, metadata, ir.error, metadataobject.DefaultObjectTypeSequence)
}

func (ir *InheritedRolesConfig) Key() string {
	return metadataobject.InheritedRolesKey
}

func (ir *InheritedRolesConfig) Filename() string {
	return "inherited_roles.yaml"
}

func (ir *InheritedRolesConfig) GetFiles() ([]string, metadataobject.ErrParsingMetadataObject) {
	rootFile := filepath.Join(ir.BaseDirectory(), ir.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, ir.error(err)
	}
	return files, nil
}

func (ir *InheritedRolesConfig) WriteDiff(opts metadataobject.WriteDiffOpts) metadataobject.ErrParsingMetadataObject {
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: ir, WriteDiffOpts: opts})
	if err != nil {
		return ir.error(err)
	}
	return nil
}

func (ir *InheritedRolesConfig) BaseDirectory() string {
	return ir.MetadataDir
}

func (ir *InheritedRolesConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(ir, err, additionalContext...)
}
