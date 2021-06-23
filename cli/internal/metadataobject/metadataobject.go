package metadataobject

import (
  "github.com/hasura/graphql-engine/cli/v2"
  "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions"
  "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/allowlist"
  apilimits "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/api_limits"
  crontriggers "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/cron_triggers"
  errors2 "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/errors"
  "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/functions"
  graphqlschemaintrospection "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/graphql_schema_introspection"
  inheritedroles "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/inherited_roles"
  "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/querycollections"
  "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/remoteschemas"
  restendpoints "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/rest_endpoints"
  "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/sources"
  "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/tables"
  "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/version"
  "gopkg.in/yaml.v2"
)

type Objects []Object

type Object interface {
	Build(metadata *yaml.MapSlice) errors2.ErrParsingMetadataObject
	Export(metadata yaml.MapSlice) (map[string][]byte, errors2.ErrParsingMetadataObject)
	CreateFiles() error
	Name() string
	Error(err error, errContext ...string) errors2.ErrParsingMetadataObject
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
	ec.Logger.Debugf("using %s as metadata directory", ec.MetadataDir)
	if ec.Config.Version >= cli.V2 && metadataDir != "" {
		// hasura core metadata objects
		if ec.HasMetadataV3 {
			if ec.Config.Version >= cli.V3 {
				objects = append(objects, version.New(ec, metadataDir))
				objects = append(objects, sources.New(ec, metadataDir))
			} else {
				objects = append(objects, version.NewV3MetadataVersion(ec, metadataDir))
				objects = append(objects, tables.NewV3MetadataTableConfig(ec, metadataDir))
				objects = append(objects, functions.NewV3MetadataFunctionConfig(ec, metadataDir))
			}
		} else {
			objects = append(objects, version.New(ec, metadataDir))
			objects = append(objects, tables.New(ec, metadataDir))
			objects = append(objects, functions.New(ec, metadataDir))
		}
		objects = append(objects, remoteschemas.New(ec, metadataDir))
		objects = append(objects, querycollections.New(ec, metadataDir))
		objects = append(objects, allowlist.New(ec, metadataDir))
		objects = append(objects, actions.New(ec, metadataDir))
		objects = append(objects, crontriggers.New(ec, metadataDir))

		if ec.HasMetadataV3 && ec.Config.Version >= cli.V3 {
			// metadata objects supported in metadata v3
			objects = append(objects, restendpoints.New(ec, metadataDir))
			objects = append(objects, inheritedroles.New(ec, metadataDir))

			// hasura pro specific metadata objects
			objects = append(objects, apilimits.New(ec, metadataDir))
			objects = append(objects, graphqlschemaintrospection.New(ec, metadataDir))
		}
	}

	return objects
}
