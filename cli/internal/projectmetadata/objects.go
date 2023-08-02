package projectmetadata

import (
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/allowlist"
	apilimits "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/api_limits"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/backend_configs"
	crontriggers "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/cron_triggers"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/functions"
	graphqlschemaintrospection "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/graphql_schema_introspection"
	inheritedroles "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/inherited_roles"
	metricsconfig "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/metrics_config"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/network"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/opentelemetry"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/querycollections"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/remoteschemas"
	restendpoints "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/rest_endpoints"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/sources"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/tables"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/version"
)

func GetMetadataObjectsWithDir(ec *cli.ExecutionContext, dir ...string) metadataobject.Objects {
	var metadataDir string
	if len(dir) == 0 {
		metadataDir = ec.MetadataDir
	} else {
		metadataDir = dir[0]
	}
	err := ec.Version.GetServerFeatureFlags()
	if err != nil {
		ec.Logger.Errorf("error determining server feature flags: %v", err)
	}
	objects := make(metadataobject.Objects, 0)
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
		objects = append(objects, network.New(ec, metadataDir))

		if ec.HasMetadataV3 && ec.Config.Version >= cli.V3 {
			// metadata objects supported in metadata v3
			objects = append(objects, restendpoints.New(ec, metadataDir))
			objects = append(objects, inheritedroles.New(ec, metadataDir))
			objects = append(objects, opentelemetry.New(ec, metadataDir))
			objects = append(objects, backendconfigs.New(ec, metadataDir))

			// hasura pro specific metadata objects
			objects = append(objects, apilimits.New(ec, metadataDir))
			objects = append(objects, graphqlschemaintrospection.New(ec, metadataDir))
			objects = append(objects, metricsconfig.New(ec, metadataDir))
		}
	}

	return objects
}
