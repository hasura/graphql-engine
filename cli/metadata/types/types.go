package types

import (
	"gopkg.in/yaml.v2"
)

type MetadataPlugins []MetadataPluginsDriver

type MetadataPluginsDriver interface {
	Build(metadata *yaml.MapSlice) error
	Export(metadata yaml.MapSlice) (map[string][]byte, error)
	CreateFiles() error
	Name() string
}
