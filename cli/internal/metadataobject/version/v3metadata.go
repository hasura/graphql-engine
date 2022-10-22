package version

import (
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v3"
)

type V3MetadataV2ConfigVersion struct {
	*VersionConfig
}

func (a *V3MetadataV2ConfigVersion) Build() (map[string]interface{}, metadataobject.ErrParsingMetadataObject) {
	return map[string]interface{}{a.Key(): 2}, nil
}

// TODO: improve naming as the method name is misleading because it gets called when we require
// version 2 metadata
func NewV3MetadataVersion(ec *cli.ExecutionContext, baseDir string) *V3MetadataV2ConfigVersion {
	err := ec.Version.GetServerFeatureFlags()
	if err != nil {
		ec.Logger.Errorf("got error while creating instance of V3MetadtaV2ConfigVersion: %v", err)
		return nil
	}
	return &V3MetadataV2ConfigVersion{
		&VersionConfig{
			MetadataDir: baseDir,
		},
	}
}

func (a *V3MetadataV2ConfigVersion) CreateFiles() error {
	v := Version{
		Version: 2,
	}
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, a.Filename()), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (a *V3MetadataV2ConfigVersion) Export(_ map[string]yaml.Node) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
	v := Version{
		// during a v3 metadata export forcefully write metadata v2
		Version: 2,
	}
	data, err := yaml.Marshal(v)
	if err != nil {
		return nil, a.error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(a.MetadataDir, a.Filename())): data,
	}, nil
}
