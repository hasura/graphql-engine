package metadata

import (
	"fmt"

	"io"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"

	"github.com/spf13/viper"

	"github.com/hasura/graphql-engine/cli/v2"
)

type ProjectMetadata struct {
	ec *cli.ExecutionContext
}

// Parse metadata in project as JSON
func (p *ProjectMetadata) Parse() (io.Reader, error) {
	var op errors.Op = "metadata.ProjectMetadata.Parse"
	r, err := getModeHandler(p.ec.MetadataMode).Parse(p)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

// Apply metadata from in the project and provide raw response from hge server
func (p *ProjectMetadata) Apply() (io.Reader, error) {
	var op errors.Op = "metadata.ProjectMetadata.Apply"
	r, err := getModeHandler(p.ec.MetadataMode).Apply(p)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

// Reload metadata on hge server and provides raw response from hge server
func (p *ProjectMetadata) Reload() (io.Reader, error) {
	var op errors.Op = "metadata.ProjectMetadata.Reload"
	metadataHandler := projectmetadata.NewHandlerFromEC(p.ec)
	r, err := metadataHandler.ReloadMetadata()
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

// GetInconsistentMetadata objects from hge server
func (p *ProjectMetadata) GetInconsistentMetadata() (io.Reader, error) {
	var op errors.Op = "metadata.ProjectMetadata.GetInconsistentMetadata"
	r, err := cli.GetCommonMetadataOps(p.ec).GetInconsistentMetadataRaw()
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

// Diff will return the differences between metadata in the project (in JSON) and on the server
func (p *ProjectMetadata) Diff() (io.Reader, error) {
	var op errors.Op = "metadata.ProjectMetadata.Diff"
	r, err := getModeHandler(p.ec.MetadataMode).Diff(p)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

// Export will return the server metadata
func (p *ProjectMetadata) Export() (io.Reader, error) {
	var op errors.Op = "metadata.ProjectMetadata.Export"
	r, err := getModeHandler(p.ec.MetadataMode).Export(p)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

type ProjectMetadataOption func(*ProjectMetadata)

func WithAdminSecret(adminSecret string) ProjectMetadataOption {
	return func(m *ProjectMetadata) {
		m.ec.Viper.Set("admin_secret", adminSecret)
	}
}

func WithEndpoint(endpoint string) ProjectMetadataOption {
	return func(m *ProjectMetadata) {
		m.ec.Viper.Set("endpoint", endpoint)
	}
}

func WithCliExtPath(path string) ProjectMetadataOption {
	return func(m *ProjectMetadata) {
		m.ec.CliExtSourceBinPath = path
	}
}

func NewProjectMetadata(projectDirectory string, opts ...ProjectMetadataOption) (*ProjectMetadata, error) {
	var op errors.Op = "metadata.NewProjectMetadata"
	ec := cli.NewExecutionContext()
	ec.ExecutionDirectory = projectDirectory
	ec.Viper = viper.New()
	ec.IsTerminal = false
	ec.Stdout = io.Discard
	ec.Stderr = io.Discard

	if err := ec.Prepare(); err != nil {
		return nil, errors.E(op, err)
	}
	p := &ProjectMetadata{ec}
	for _, opt := range opts {
		opt(p)
	}

	if err := ec.Validate(); err != nil {
		return nil, errors.E(op, err)
	}
	if ec.Config.Version <= cli.V1 {
		return nil, errors.E(op, fmt.Errorf("config %v is not supported", ec.Config.Version))
	}
	return p, nil
}
