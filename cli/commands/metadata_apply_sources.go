package commands

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/spf13/cobra"
	"gopkg.in/yaml.v3"
)

func newMetadataApplySourcesCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataApplySourcesOptions{
		EC: ec,
	}

	metadataApplySourcesCmd := &cobra.Command{
		Use:   "apply-sources",
		Short: "Apply only source configurations from a config v3 metadata project",
		Long: `This command applies only source configurations from metadata/databases/databases.yaml for config v3 projects.
It creates missing sources and updates existing sources without applying tracked tables, functions, or relationships.

This is useful for deployment workflows where sources should be configured before running migrations and before applying full metadata.

Further reading:
- https://hasura.io/docs/latest/migrations-metadata-seeds/manage-metadata/
- https://hasura.io/docs/latest/migrations-metadata-seeds/metadata-format/
`,
		Example: `  # Apply only source configurations from metadata/databases/databases.yaml:
  hasura metadata apply-sources

  # Apply source configurations to an instance specified by the flag:
  hasura metadata apply-sources --endpoint "<endpoint>"

  # Use with admin secret:
  hasura metadata apply-sources --admin-secret "<admin-secret>"`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			ec.Spin("Applying source configurations...")
			err := opts.Run()
			ec.Spinner.Stop()
			if err != nil {
				return errors.E(op, err)
			}
			opts.EC.Logger.Info("Source configurations applied")
			return nil
		},
	}

	return metadataApplySourcesCmd
}

type MetadataApplySourcesOptions struct {
	EC *cli.ExecutionContext
}

type metadataApplySourceConfig struct {
	Name          string      `json:"name"`
	Kind          string      `json:"kind"`
	Configuration interface{} `json:"configuration"`
	Customization interface{} `json:"customization,omitempty"`
	HealthCheck   interface{} `json:"health_check,omitempty"`
}

type metadataApplySourcesExportMetadataResponse struct {
	Sources []metadataApplySourceConfig `json:"sources"`
}

func (o *MetadataApplySourcesOptions) Run() error {
	var op errors.Op = "commands.MetadataApplySourcesOptions.Run"
	if err := o.validateProject(); err != nil {
		return errors.E(op, err)
	}

	localSources, err := loadMetadataApplySources(filepath.Join(o.EC.MetadataDir, "databases"))
	if err != nil {
		return errors.E(op, err)
	}

	serverSources, err := o.exportServerSources()
	if err != nil {
		return errors.E(op, err)
	}

	for _, source := range localSources {
		if _, err := metadataApplySourcesRequestType(source.Kind, "add"); err != nil {
			return errors.E(op, err)
		}
		if existingSource, ok := serverSources[source.Name]; ok && existingSource.Kind != source.Kind {
			return errors.E(op, fmt.Errorf("source %q already exists on server with backend %q, found local backend %q", source.Name, existingSource.Kind, source.Kind))
		}
	}

	for _, source := range localSources {
		requestType, err := metadataApplySourcesRequestType(source.Kind, "add")
		if err != nil {
			return errors.E(op, err)
		}
		if err := o.applySource(source, requestType, serverSources); err != nil {
			return errors.E(op, err)
		}
	}

	return nil
}

func (o *MetadataApplySourcesOptions) validateProject() error {
	var op errors.Op = "commands.MetadataApplySourcesOptions.validateProject"
	if o.EC.Config.Version < cli.V3 || !o.EC.HasMetadataV3 {
		return errors.E(op, fmt.Errorf("metadata apply-sources is only supported for config v3 projects with source configuration in metadata/databases/databases.yaml"))
	}
	return nil
}

func loadMetadataApplySources(sourcesMetadataDirectory string) ([]metadataApplySourceConfig, error) {
	var op errors.Op = "commands.loadMetadataApplySources"
	sourceFile := filepath.Join(sourcesMetadataDirectory, "databases.yaml")
	sourceBytes, err := metadataobject.ReadMetadataFile(sourceFile)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("reading source configurations: %w", err))
	}

	var sourceNode yaml.Node
	if err := yaml.Unmarshal(sourceBytes, metadatautil.NewYamlDecoder(
		metadatautil.YamlDecoderOpts{IncludeTagBaseDirectory: sourcesMetadataDirectory},
		&sourceNode,
	)); err != nil {
		return nil, errors.E(op, fmt.Errorf("parsing source configurations: %w", err))
	}

	resolvedSourceBytes, err := yaml.Marshal(&sourceNode)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("encoding resolved source configurations: %w", err))
	}

	sourceJSON, err := metadatautil.YAMLToJSON(resolvedSourceBytes)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("parsing resolved source configurations as json: %w", err))
	}

	var localSources []metadataApplySourceConfig
	if err := json.Unmarshal(sourceJSON, &localSources); err != nil {
		return nil, errors.E(op, fmt.Errorf("decoding source configurations: %w", err))
	}

	return localSources, nil
}

func (o *MetadataApplySourcesOptions) exportServerSources() (map[string]metadataApplySourceConfig, error) {
	var op errors.Op = "commands.MetadataApplySourcesOptions.exportServerSources"
	metadata, err := o.EC.APIClient.V1Metadata.ExportMetadata()
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("exporting metadata from server: %w", err))
	}

	metadataBytes, err := ioutil.ReadAll(metadata)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("reading metadata from response: %w", err))
	}

	var exportMetadataResponse metadataApplySourcesExportMetadataResponse
	if err := json.Unmarshal(metadataBytes, &exportMetadataResponse); err != nil {
		return nil, errors.E(op, fmt.Errorf("decoding server metadata: %w", err))
	}

	serverSources := make(map[string]metadataApplySourceConfig, len(exportMetadataResponse.Sources))
	for _, source := range exportMetadataResponse.Sources {
		serverSources[source.Name] = source
	}
	return serverSources, nil
}

func (o *MetadataApplySourcesOptions) applySource(source metadataApplySourceConfig, addRequestType string, serverSources map[string]metadataApplySourceConfig) error {
	var op errors.Op = "commands.MetadataApplySourcesOptions.applySource"
	_, sourceExistsOnServer := serverSources[source.Name]

	addSourceArgs := map[string]interface{}{
		"name":          source.Name,
		"configuration": source.Configuration,
	}
	if sourceExistsOnServer {
		addSourceArgs["replace_configuration"] = true
	}
	if source.Customization != nil {
		addSourceArgs["customization"] = source.Customization
	}
	if !sourceExistsOnServer && source.HealthCheck != nil {
		addSourceArgs["health_check"] = source.HealthCheck
	}

	if err := o.sendMetadataRequest(hasura.RequestBody{Type: addRequestType, Args: addSourceArgs}); err != nil {
		return errors.E(op, fmt.Errorf("applying source %q: %w", source.Name, err))
	}

	if sourceExistsOnServer && source.HealthCheck != nil {
		updateRequestType, err := metadataApplySourcesRequestType(source.Kind, "update")
		if err != nil {
			return errors.E(op, err)
		}
		if err := o.sendMetadataRequest(hasura.RequestBody{
			Type: updateRequestType,
			Args: map[string]interface{}{
				"name":         source.Name,
				"health_check": source.HealthCheck,
			},
		}); err != nil {
			return errors.E(op, fmt.Errorf("updating health check for source %q: %w", source.Name, err))
		}
	}

	return nil
}

func (o *MetadataApplySourcesOptions) sendMetadataRequest(requestBody hasura.RequestBody) error {
	var op errors.Op = "commands.MetadataApplySourcesOptions.sendMetadataRequest"
	response, body, err := o.EC.APIClient.V1Metadata.Send(requestBody)
	if err != nil {
		return errors.E(op, err)
	}
	if response.StatusCode != http.StatusOK {
		responseBody, readErr := ioutil.ReadAll(body)
		if readErr != nil {
			return errors.E(op, fmt.Errorf("reading error response: %w", readErr))
		}
		return errors.E(op, errors.KindHasuraAPI, string(responseBody))
	}
	return nil
}

func metadataApplySourcesRequestType(kind string, operation string) (string, error) {
	var op errors.Op = "commands.metadataApplySourcesRequestType"
	var prefix string
	switch hasura.SourceKind(kind) {
	case hasura.SourceKindPG:
		prefix = "pg"
	case hasura.SourceKindMSSQL:
		prefix = "mssql"
	case hasura.SourceKindCitus:
		prefix = "citus"
	case hasura.SourceKindCockroach:
		prefix = "cockroach"
	case hasura.SourceKindBigQuery:
		prefix = "bigquery"
	default:
		return "", errors.E(op, fmt.Errorf("unsupported source backend %q", kind))
	}
	return fmt.Sprintf("%s_%s_source", prefix, operation), nil
}
