package version

import (
	"github.com/hasura/graphql-engine/cli"
	"gopkg.in/yaml.v2"
)

type V3MetadataVersion struct {
	*VersionConfig
}

func (a *V3MetadataVersion) Build(metadata *yaml.MapSlice) error {
	item := yaml.MapItem{
		Key: "version",
		// Force version 2
		Value: 2,
	}
	*metadata = append(*metadata, item)
	return nil
}

func NewV3MetadataVersion(ec *cli.ExecutionContext, baseDir string) *V3MetadataVersion {
	ec.Version.GetServerFeatureFlags()
	return &V3MetadataVersion{
		&VersionConfig{
			MetadataDir: baseDir,
		},
	}
}
