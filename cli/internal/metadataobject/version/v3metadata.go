package version

import (
	"path/filepath"

	"github.com/hasura/graphql-engine/cli"
	"gopkg.in/yaml.v2"
)

type V3MetadataV2ConfigVersion struct {
	*VersionConfig
}

func (a *V3MetadataV2ConfigVersion) Build(metadata *yaml.MapSlice) error {
	item := yaml.MapItem{
		Key: "version",
		// Force version 2
		Value: 2,
	}
	*metadata = append(*metadata, item)
	return nil
}

func NewV3MetadataVersion(ec *cli.ExecutionContext, baseDir string) *V3MetadataV2ConfigVersion {
	ec.Version.GetServerFeatureFlags()
	return &V3MetadataV2ConfigVersion{
		&VersionConfig{
			MetadataDir: baseDir,
		},
	}
}
func (a *V3MetadataV2ConfigVersion) Export(_ yaml.MapSlice) (map[string][]byte, error) {
	v := Version{
		// during a v3 metadata export forcefully write metadata v2
		Version: 2,
	}
	data, err := yaml.Marshal(v)
	if err != nil {
		return nil, err
	}
	return map[string][]byte{
		filepath.Join(a.MetadataDir, fileName): data,
	}, nil
}
