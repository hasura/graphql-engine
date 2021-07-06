package metadata

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"

	"github.com/hasura/graphql-engine/cli/v2/commands"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/spf13/viper"

	"github.com/hasura/graphql-engine/cli/v2"
)

type ProjectMetadata struct {
	ec *cli.ExecutionContext
}

// Parse metadata in project as JSON
func (p *ProjectMetadata) Parse() (io.Reader, error) {
	metadataHandler := metadataobject.NewHandlerFromEC(p.ec)
	jsonMetadata, err := metadataHandler.MakeJSONMetadata()
	if err != nil {
		return nil, fmt.Errorf("parsing project metadata to json failed: %w", err)
	}
	return bytes.NewReader(jsonMetadata), nil
}

// Apply metadata from in the project and provide raw response from hge server
func (p *ProjectMetadata) Apply() (io.Reader, error) {
	metadataHandler := metadataobject.NewHandlerFromEC(p.ec)
	if p.ec.Config.Version == cli.V2 {
		r, err := metadataHandler.V1ApplyMetadata()
		if err != nil {
			return nil, err
		}
		return r, nil
	}
	if p.ec.Config.Version >= cli.V3 {
		replaceMetadataResponse, err := metadataHandler.V2ApplyMetadata()
		if err != nil {
			return nil, err
		}
		b := new(bytes.Buffer)
		if err := json.NewEncoder(b).Encode(replaceMetadataResponse); err != nil {
			return nil, fmt.Errorf("encoding json reponse from server: %w", err)
		}
		return b, nil
	}
	return nil, nil
}

// Reload metadata on hge server and provides raw response from hge server
func (p *ProjectMetadata) Reload() (io.Reader, error) {
	metadataHandler := metadataobject.NewHandlerFromEC(p.ec)
	return metadataHandler.ReloadMetadata()
}

// GetInconsistentMetadata objects from hge server
func (p *ProjectMetadata) GetInconsistentMetadata() (io.Reader, error) {
	return cli.GetCommonMetadataOps(p.ec).GetInconsistentMetadataRaw()
}

// Diff will return the differences between metadata in the project (in JSON) and on the server
func (p *ProjectMetadata) Diff() (io.Reader, error) {
	w := new(bytes.Buffer)
	opts := &commands.MetadataDiffOptions{
		EC:           p.ec,
		Output:       w,
		DisableColor: true,
	}
	if err := opts.Run(); err != nil {
		return nil, err
	}
	return w, nil
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
