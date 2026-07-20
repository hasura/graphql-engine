package commands

import (
	"bytes"
	"encoding/json"
	stderrors "errors"
	"fmt"
	"io"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/sources"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/spf13/cobra"
	"go.yaml.in/yaml/v3"
)

func newMetadataApplyDataSourcesCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataApplyDataSourcesOptions{
		EC: ec,
	}

	metadataApplyDataSourcesCmd := &cobra.Command{
		Use:   "apply-data-sources",
		Short: "Apply only the data sources (connection configuration) from the project metadata",
		Long: `This command registers/updates only the data sources (their connection configuration and
customization) from the project metadata, without touching the rest of the metadata such as tables,
relationships, permissions, actions etc.

It is intended to be run as the first step of a migrations/metadata bootstrap, before applying
migrations, so that:

  1. Migrations can be applied to sources (including a brand new source added in the same change set), and
  2. The full metadata (which makes the new schema live on the GraphQL API) is only applied afterwards,
     once the underlying database schema actually exists.

The command is idempotent and non-destructive: existing sources only have their connection configuration
and customization updated (tracked tables, functions etc. are preserved), new sources are added with an
empty set of tracked objects, and sources are never dropped. Dropping a source is handled by a subsequent
full 'hasura metadata apply'.

This requires a config v3 project and connectivity to the data source(s).`,
		Example: `  # Apply only the data sources from the project metadata:
  hasura metadata apply-data-sources

  # Use with admin secret:
  hasura metadata apply-data-sources --admin-secret "<admin-secret>"

  # Apply to an instance specified by the flag:
  hasura metadata apply-data-sources --endpoint "<endpoint>"

  # Show the metadata API request without applying it:
  hasura metadata apply-data-sources --dry-run`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")

			if !opts.DryRun {
				ec.Spin("Applying data sources...")
			}

			err := opts.Run()

			ec.Spinner.Stop()

			if err != nil {
				return errors.E(op, err)
			}

			return nil
		},
	}

	f := metadataApplyDataSourcesCmd.Flags()
	f.BoolVar(
		&opts.DryRun,
		"dry-run",
		false,
		"show the metadata API request that would be sent to the server, without applying it",
	)

	return metadataApplyDataSourcesCmd
}

type MetadataApplyDataSourcesOptions struct {
	EC *cli.ExecutionContext

	DryRun bool
}

func (o *MetadataApplyDataSourcesOptions) Run() error {
	var op errors.Op = "commands.MetadataApplyDataSourcesOptions.Run"

	// Only config v3 projects have the concept of "sources" in metadata.
	if o.EC.Config.Version < cli.V3 || !o.EC.HasMetadataV3 {
		return errors.E(
			op,
			stderrors.New("metadata apply-data-sources is only supported on config v3 projects"),
		)
	}

	requestBody, numSources, err := buildApplyDataSourcesRequest(o.EC)
	if err != nil {
		return errors.E(op, err)
	}

	if numSources == 0 {
		o.EC.Logger.Info("No data sources found in project metadata, nothing to apply")

		return nil
	}

	if o.DryRun {
		out := new(bytes.Buffer)

		err := writeByOutputFormat(out, requestBody, rawOutputFormatJSON)
		if err != nil {
			return errors.E(op, err)
		}

		fmt.Fprintln(o.EC.Stdout, out.String())

		return nil
	}

	resp, body, err := o.EC.APIClient.V1Metadata.SendCommonMetadataOperation(
		json.RawMessage(requestBody),
	)
	if err != nil {
		return errors.E(op, err)
	}

	if resp.StatusCode != http.StatusOK {
		b, err := io.ReadAll(body)
		if err != nil {
			return errors.E(op, errors.KindHasuraAPI, err)
		}

		return errors.E(
			op,
			errors.KindHasuraAPI,
			fmt.Errorf("applying data sources failed: %s", string(b)),
		)
	}

	o.EC.Logger.Infof("Data sources applied (%d)", numSources)

	return nil
}

// addSourceArgs holds only the fields accepted by the server's `<kind>_add_source` metadata API.
// Notably it excludes tables/functions/permissions so that this operation only touches the source's
// connection configuration and never the rest of the source's metadata.
type addSourceArgs struct {
	Name                 string    `yaml:"name"`
	Configuration        yaml.Node `yaml:"configuration"`
	ReplaceConfiguration bool      `yaml:"replace_configuration"`
	Customization        yaml.Node `yaml:"customization,omitempty"`
	HealthCheck          yaml.Node `yaml:"health_check,omitempty"`
}

type metadataAPIRequest struct {
	Type string        `yaml:"type"`
	Args addSourceArgs `yaml:"args"`
}

type bulkMetadataAPIRequest struct {
	Type string               `yaml:"type"`
	Args []metadataAPIRequest `yaml:"args"`
}

// buildApplyDataSourcesRequest reads the project's source metadata and builds a single `bulk` metadata
// API request consisting of one `<kind>_add_source` operation per source (with replace_configuration
// set so the operation is an idempotent upsert). It returns the JSON request body and the number of
// sources found.
func buildApplyDataSourcesRequest(ec *cli.ExecutionContext) ([]byte, int, error) {
	var op errors.Op = "commands.buildApplyDataSourcesRequest"

	sourceObject := sources.New(ec, ec.MetadataDir)

	built, err := sourceObject.Build()
	if err != nil {
		// A missing databases.yaml simply means there are no sources to apply.
		if stderrors.Is(err, metadataobject.ErrMetadataFileNotFound) {
			return nil, 0, nil
		}

		return nil, 0, errors.E(op, err)
	}

	projectSources, ok := built[sourceObject.Key()].([]sources.Source)
	if !ok || len(projectSources) == 0 {
		return nil, 0, nil
	}

	return buildAddSourceBulkRequest(projectSources)
}

// buildAddSourceBulkRequest turns the project's sources into a single `bulk` metadata API request of
// `<kind>_add_source` operations (one per source) carrying only the connection-level fields.
func buildAddSourceBulkRequest(projectSources []sources.Source) ([]byte, int, error) {
	var op errors.Op = "commands.buildAddSourceBulkRequest"

	requests := make([]metadataAPIRequest, 0, len(projectSources))
	for _, source := range projectSources {
		if len(source.Name) == 0 || len(source.Kind) == 0 {
			return nil, 0, errors.E(
				op,
				stderrors.New("found a source with a missing name or kind in project metadata"),
			)
		}

		requests = append(requests, metadataAPIRequest{
			Type: addSourceCommandName(source.Kind),
			Args: addSourceArgs{
				Name:                 source.Name,
				Configuration:        source.Configuration,
				ReplaceConfiguration: true,
				Customization:        source.Customization,
				HealthCheck:          source.HealthCheck,
			},
		})
	}

	// The source metadata structs carry yaml.Node values which cannot be marshalled directly with
	// encoding/json (see projectmetadata.Metadata.JSON), so go via YAML and convert to JSON.
	yamlBody, err := yaml.Marshal(bulkMetadataAPIRequest{Type: "bulk", Args: requests})
	if err != nil {
		return nil, 0, errors.E(op, err)
	}

	jsonBody, err := metadatautil.YAMLToJSON(yamlBody)
	if err != nil {
		return nil, 0, errors.E(op, err)
	}

	return jsonBody, len(requests), nil
}

// addSourceCommandName returns the metadata API command name used to add/update a source of the given
// kind. Native backends use a backend-specific prefix (e.g. postgres -> pg_add_source); every other
// kind (mssql, citus, cockroach, bigquery and data connector agents) uses "<kind>_add_source".
func addSourceCommandName(kind string) string {
	if hasura.SourceKind(kind) == hasura.SourceKindPG {
		return "pg_add_source"
	}

	return kind + "_add_source"
}
