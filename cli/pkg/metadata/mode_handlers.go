package metadata

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/commands"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"

)

type modeHandler interface {
	Apply(*ProjectMetadata) (io.Reader, error)
	Diff(*ProjectMetadata) (io.Reader, error)
	Parse(*ProjectMetadata) (io.Reader, error)
}

func getModeHandler(mode cli.MetadataMode) modeHandler {
	switch mode {
	case cli.MetadataModeYAML:
		return &metadataModeYAMLHandler{}
	case cli.MetadataModeJSON:
		return &metadataModeJSONHandler{}
	default:
		return &metadataModeDirectoryHandler{}
	}
}

type metadataModeDirectoryHandler struct{}

func (m *metadataModeDirectoryHandler) Apply(p *ProjectMetadata) (io.Reader, error) {
	metadataHandler := projectmetadata.NewHandlerFromEC(p.ec)
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

func (m *metadataModeDirectoryHandler) Parse(p *ProjectMetadata) (io.Reader, error) {
	metadataHandler := projectmetadata.NewHandlerFromEC(p.ec)
	jsonMetadata, err := metadataHandler.BuildJSONMetadata()
	if err != nil {
		return nil, fmt.Errorf("parsing project metadata to json failed: %w", err)
	}
	return bytes.NewReader(jsonMetadata), nil
}

func (m *metadataModeDirectoryHandler) Diff(p *ProjectMetadata) (io.Reader, error) {
	return diff(p)
}

func diff(p *ProjectMetadata) (io.Reader, error) {
	w := new(bytes.Buffer)
	opts := &commands.MetadataDiffOptions{
		EC:           p.ec,
		Output:       w,
		DisableColor: true,
		DiffType:     string(commands.DifftypeYAML),
	}
	if err := opts.Run(); err != nil {
		return nil, err
	}
	return w, nil
}

type metadataModeJSONHandler struct{}

func (m *metadataModeJSONHandler) Apply(p *ProjectMetadata) (io.Reader, error) {
	return apply(p, p.ec.MetadataMode)
}

func (m *metadataModeJSONHandler) Parse(p *ProjectMetadata) (io.Reader, error) {
	metadataJSON, err := ioutil.ReadFile(p.ec.MetadataFile)
	if err != nil {
		return nil, fmt.Errorf("reading metadata file: %w", err)
	}
	return bytes.NewReader(metadataJSON), nil
}

func (m *metadataModeJSONHandler) Diff(p *ProjectMetadata) (io.Reader, error) {
	return diff(p)
}

type metadataModeYAMLHandler struct{}

func (m *metadataModeYAMLHandler) Apply(p *ProjectMetadata) (io.Reader, error) {
	return apply(p, p.ec.MetadataMode)
}

func (m *metadataModeYAMLHandler) Parse(p *ProjectMetadata) (io.Reader, error) {
	metadataYAML, err := ioutil.ReadFile(p.ec.MetadataFile)
	if err != nil {
		return nil, fmt.Errorf("reading metadata file: %w", err)
	}
	metadataJSON, err := metadatautil.YAMLToJSON(metadataYAML)
	if err != nil {
		return nil, fmt.Errorf("parsing local yaml metadata as json: %w", err)
	}
	return bytes.NewReader(metadataJSON), nil
}

func (m *metadataModeYAMLHandler) Diff(p *ProjectMetadata) (io.Reader, error) {
	return diff(p)
}

func apply(p *ProjectMetadata, mode cli.MetadataMode) (io.Reader, error) {
	var localMetadataBytes []byte
	var err error

	localMetadataBytes, err = ioutil.ReadFile(p.ec.MetadataFile)
	if err != nil {
		return nil, fmt.Errorf("reading metadata file: %w", err)
	}
	if mode == cli.MetadataModeYAML {
		localMetadataBytes, err = metadatautil.YAMLToJSON(localMetadataBytes)
		if err != nil {
			return nil, fmt.Errorf("parsing yaml metadata to json: %w", err)
		}
	}
	var metadata interface{}
	err = json.Unmarshal(localMetadataBytes, &metadata)
	if err != nil {
		return nil, fmt.Errorf("parsing metadata as json: %w", err)
	}
	if p.ec.Config.Version == cli.V2 {
		return cli.GetCommonMetadataOps(p.ec).ReplaceMetadata(bytes.NewReader(localMetadataBytes))
	}
	r, err := p.ec.APIClient.V1Metadata.V2ReplaceMetadata(hasura.V2ReplaceMetadataArgs{
		AllowInconsistentMetadata: true,
		Metadata:                  metadata,
	})
	if err != nil {
		return nil, fmt.Errorf("applying metadata: %w", err)
	}
	b := new(bytes.Buffer)
	if err := json.NewEncoder(b).Encode(r); err != nil {
		return nil, fmt.Errorf("encoding json reponse from server: %w", err)
	}
	return b, nil
}
