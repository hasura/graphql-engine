package inheritedroles

import (
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
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

func (ir *InheritedRolesConfig) Build() (map[string]interface{}, error) {
	var op errors.Op = "inheritedroles.InheritedRolesConfig.Build"
	data, err := metadataobject.ReadMetadataFile(filepath.Join(ir.MetadataDir, ir.Filename()))
	if err != nil {
		return nil, errors.E(op, ir.error(err))
	}
	var obj []yaml.Node
	err = yaml.Unmarshal(data, &obj)
	if err != nil {
		return nil, errors.E(op, errors.KindBadInput, ir.error(err))
	}
	return map[string]interface{}{ir.Key(): obj}, nil
}

func (ir *InheritedRolesConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "inheritedroles.InheritedRolesConfig.Export"
	b, err := metadataobject.DefaultExport(ir, metadata, ir.error, metadataobject.DefaultObjectTypeSequence)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return b, nil
}

func (ir *InheritedRolesConfig) Key() string {
	return metadataobject.InheritedRolesKey
}

func (ir *InheritedRolesConfig) Filename() string {
	return "inherited_roles.yaml"
}

func (ir *InheritedRolesConfig) GetFiles() ([]string, error) {
	var op errors.Op = "inheritedroles.InheritedRolesConfig.GetFiles"
	rootFile := filepath.Join(ir.BaseDirectory(), ir.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, errors.E(op, ir.error(err))
	}
	return files, nil
}

func (ir *InheritedRolesConfig) WriteDiff(opts metadataobject.WriteDiffOpts) error {
	var op errors.Op = "inheritedroles.InheritedRolesConfig.WriteDiff"
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: ir, WriteDiffOpts: opts})
	if err != nil {
		return errors.E(op, ir.error(err))
	}
	return nil
}

func (ir *InheritedRolesConfig) BaseDirectory() string {
	return ir.MetadataDir
}

func (ir *InheritedRolesConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(ir, err, additionalContext...)
}
