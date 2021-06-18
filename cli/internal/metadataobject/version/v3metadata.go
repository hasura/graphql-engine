package version

import (
	"path/filepath"

	errors2 "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/errors"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v2"
)

type V3MetadataV2ConfigVersion struct {
	*VersionConfig
}

func (a *V3MetadataV2ConfigVersion) Build(metadata *yaml.MapSlice) errors2.ErrParsingMetadataObject {
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
func (a *V3MetadataV2ConfigVersion) Export(_ yaml.MapSlice) (map[string][]byte, errors2.ErrParsingMetadataObject) {
	v := Version{
		// during a v3 metadata export forcefully write metadata v2
		Version: 2,
	}
	data, err := yaml.Marshal(v)
	if err != nil {
		return nil, a.Error(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(a.MetadataDir, fileName)): data,
	}, nil
}
