package metadata

import (
	"fmt"

	"io"

	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"

	"github.com/spf13/viper"

	"github.com/hasura/graphql-engine/cli/v2"
)

type ProjectMetadata struct {
	ec *cli.ExecutionContext
}

// Parse metadata in project as JSON
func (p *ProjectMetadata) Parse() (io.Reader, error) {
	return getModeHandler(p.ec.MetadataMode).Parse(p)
}

// Apply metadata from in the project and provide raw response from hge server
func (p *ProjectMetadata) Apply() (io.Reader, error) {
	return getModeHandler(p.ec.MetadataMode).Apply(p)
}

// Reload metadata on hge server and provides raw response from hge server
func (p *ProjectMetadata) Reload() (io.Reader, error) {
	metadataHandler := projectmetadata.NewHandlerFromEC(p.ec)
	return metadataHandler.ReloadMetadata()
}

// GetInconsistentMetadata objects from hge server
func (p *ProjectMetadata) GetInconsistentMetadata() (io.Reader, error) {
	return cli.GetCommonMetadataOps(p.ec).GetInconsistentMetadataRaw()
}

// Diff will return the differences between metadata in the project (in JSON) and on the server
func (p *ProjectMetadata) Diff() (io.Reader, error) {
	return getModeHandler(p.ec.MetadataMode).Diff(p)
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
	ec := cli.NewExecutionContext()
	ec.ExecutionDirectory = projectDirectory
	ec.Viper = viper.New()
	ec.IsTerminal = false
	ec.Stdout = io.Discard
	ec.Stderr = io.Discard

	if err := ec.Prepare(); err != nil {
		return nil, err
	}
	p := &ProjectMetadata{ec}
	for _, opt := range opts {
		opt(p)
	}

	if err := ec.Validate(); err != nil {
		return nil, err
	}
	if ec.Config.Version <= cli.V1 {
		return nil, fmt.Errorf("config %v is not supported", ec.Config.Version)
	}
	return p, nil
}
