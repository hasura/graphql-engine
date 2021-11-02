package version

import (
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v2"
)

type V3MetadataV2ConfigVersion struct {
	*VersionConfig
}

func (a *V3MetadataV2ConfigVersion) Build(metadata *yaml.MapSlice) metadataobject.ErrParsingMetadataObject {
	item := yaml.MapItem{
		Key: "version",
		// Force version 2
		Value: 2,
	}
	*metadata = append(*metadata, item)
	return nil
}

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
func (a *V3MetadataV2ConfigVersion) Export(_ yaml.MapSlice) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {
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
