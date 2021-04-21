package metadataobject

import (
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/internal/metadataobject/actions"
	"github.com/hasura/graphql-engine/cli/internal/metadataobject/allowlist"
	apilimits "github.com/hasura/graphql-engine/cli/internal/metadataobject/api_limits"
	crontriggers "github.com/hasura/graphql-engine/cli/internal/metadataobject/cron_triggers"
	"github.com/hasura/graphql-engine/cli/internal/metadataobject/functions"
	inheritedroles "github.com/hasura/graphql-engine/cli/internal/metadataobject/inherited_roles"
	"github.com/hasura/graphql-engine/cli/internal/metadataobject/querycollections"
	"github.com/hasura/graphql-engine/cli/internal/metadataobject/remoteschemas"
	restendpoints "github.com/hasura/graphql-engine/cli/internal/metadataobject/rest_endpoints"
	"github.com/hasura/graphql-engine/cli/internal/metadataobject/sources"
	"github.com/hasura/graphql-engine/cli/internal/metadataobject/tables"
	"github.com/hasura/graphql-engine/cli/internal/metadataobject/version"
	"gopkg.in/yaml.v2"
)

type Objects []Object

type Object interface {
	Build(metadata *yaml.MapSlice) error
	Export(metadata yaml.MapSlice) (map[string][]byte, error)
	CreateFiles() error
	Name() string
}

func GetMetadataObjectsWithDir(ec *cli.ExecutionContext, dir ...string) Objects {
	var metadataDir string
	if len(dir) == 0 {
		metadataDir = ec.MetadataDir
	} else {
		metadataDir = dir[0]
	}
	ec.Version.GetServerFeatureFlags()
	objects := make(Objects, 0)
	if ec.Config.Version >= cli.V2 && metadataDir != "" {
		objects = append(objects, version.New(ec, metadataDir))
		objects = append(objects, querycollections.New(ec, metadataDir))
		objects = append(objects, allowlist.New(ec, metadataDir))
		objects = append(objects, remoteschemas.New(ec, metadataDir))
		objects = append(objects, actions.New(ec, metadataDir))
		objects = append(objects, crontriggers.New(ec, metadataDir))
		objects = append(objects, restendpoints.New(ec, metadataDir))
		objects = append(objects, inheritedroles.New(ec, metadataDir))
		objects = append(objects, apilimits.New(ec, metadataDir))

		if ec.HasMetadataV3 {
			if ec.Config.Version >= cli.V3 {
				objects = append(objects, sources.New(ec, metadataDir))
			} else {
				objects = append(objects, tables.NewV3MetadataTableConfig(ec, metadataDir))
				objects = append(objects, functions.NewV3MetadataFunctionConfig(ec, metadataDir))
				objects = append(objects, version.NewV3MetadataVersion(ec, metadataDir))
			}
		} else {
			objects = append(objects, tables.New(ec, metadataDir))
			objects = append(objects, functions.New(ec, metadataDir))
		}
	}

	return objects
}
