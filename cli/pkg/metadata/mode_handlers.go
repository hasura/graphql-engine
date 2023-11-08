package metadata

import (
	"bytes"
	"encoding/json"
	"fmt"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/commands"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"
	"io"
	"io/ioutil"
)

type modeHandler interface {
	Apply(*ProjectMetadata) (io.Reader, error)
	Diff(*ProjectMetadata) (io.Reader, error)
	Parse(*ProjectMetadata) (io.Reader, error)
	Export(*ProjectMetadata) (io.Reader, error)
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
	var op errors.Op = "metadata.metadataModeDirectoryHandler.Apply"
	metadataHandler := projectmetadata.NewHandlerFromEC(p.ec)
	if p.ec.Config.Version == cli.V2 {
		r, err := metadataHandler.V1ApplyMetadata()
		if err != nil {
			return nil, errors.E(op, err)
		}
		return r, nil
	}
	if p.ec.Config.Version >= cli.V3 {
		replaceMetadataResponse, err := metadataHandler.V2ApplyMetadata(false)
		if err != nil {
			return nil, errors.E(op, err)
		}
		b := new(bytes.Buffer)
		if err := json.NewEncoder(b).Encode(replaceMetadataResponse); err != nil {
			return nil, errors.E(op, fmt.Errorf("encoding json response from server: %w", err))
		}
		return b, nil
	}
	return nil, nil
}

func (m *metadataModeDirectoryHandler) Parse(p *ProjectMetadata) (io.Reader, error) {
	var op errors.Op = "metadata.metadataModeDirectoryHandler.Parse"
	metadataHandler := projectmetadata.NewHandlerFromEC(p.ec)
	jsonMetadata, err := metadataHandler.BuildJSONMetadata()
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("parsing project metadata to json failed: %w", err))
	}
	return bytes.NewReader(jsonMetadata), nil
}

func (m *metadataModeDirectoryHandler) Diff(p *ProjectMetadata) (io.Reader, error) {
	var op errors.Op = "metadata.metadataModeDirectoryHandler.Diff"
	r, err := diff(p)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

func (m *metadataModeDirectoryHandler) Export(p *ProjectMetadata) (io.Reader, error) {
	var op errors.Op = "metadata.metadataModeDirectoryHandler.Export"
	r, err := export(p, p.ec.MetadataMode)
	if err != nil {
		return nil, errors.E(op, err)
	}

	return r, nil
}

func diff(p *ProjectMetadata) (io.Reader, error) {
	var op errors.Op = "metadata.diff"
	w := new(bytes.Buffer)
	opts := &commands.MetadataDiffOptions{
		EC:           p.ec,
		Output:       w,
		DisableColor: true,
		DiffType:     string(commands.DifftypeYAML),
	}
	if err := opts.Run(); err != nil {
		return nil, errors.E(op, err)
	}
	return w, nil
}

type metadataModeJSONHandler struct{}

func (m *metadataModeJSONHandler) Apply(p *ProjectMetadata) (io.Reader, error) {
	var op errors.Op = "metadata.metadataModeJSONHandler.Apply"
	r, err := apply(p, p.ec.MetadataMode)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

func (m *metadataModeJSONHandler) Parse(p *ProjectMetadata) (io.Reader, error) {
	var op errors.Op = "metadata.metadataModeJSONHandler.Parse"
	metadataJSON, err := ioutil.ReadFile(p.ec.MetadataFile)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("reading metadata file: %w", err))
	}
	return bytes.NewReader(metadataJSON), nil
}

func (m *metadataModeJSONHandler) Diff(p *ProjectMetadata) (io.Reader, error) {
	var op errors.Op = "metadata.metadataModeJSONHandler.Diff"
	r, err := diff(p)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

func (m *metadataModeJSONHandler) Export(p *ProjectMetadata) (io.Reader, error) {
	var op errors.Op = "metadata.metadataModeDirectoryHandler.Export"
	r, err := export(p, p.ec.MetadataMode)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

type metadataModeYAMLHandler struct{}

func (m *metadataModeYAMLHandler) Apply(p *ProjectMetadata) (io.Reader, error) {
	var op errors.Op = "metadata.metadataModeYAMLHandler.Apply"
	r, err := apply(p, p.ec.MetadataMode)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

func (m *metadataModeYAMLHandler) Parse(p *ProjectMetadata) (io.Reader, error) {
	var op errors.Op = "metadata.metadataModeYAMLHandler.Parse"
	metadataYAML, err := ioutil.ReadFile(p.ec.MetadataFile)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("reading metadata file: %w", err))
	}
	metadataJSON, err := metadatautil.YAMLToJSON(metadataYAML)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("parsing local yaml metadata as json: %w", err))
	}
	return bytes.NewReader(metadataJSON), nil
}

func (m *metadataModeYAMLHandler) Diff(p *ProjectMetadata) (io.Reader, error) {
	var op errors.Op = "metadata.metadataModeYAMLHandler.Diff"
	r, err := diff(p)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

func (m *metadataModeYAMLHandler) Export(p *ProjectMetadata) (io.Reader, error) {
	var op errors.Op = "metadata.metadataModeYAMLHandler.Export"
	r, err := export(p, p.ec.MetadataMode)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

func apply(p *ProjectMetadata, mode cli.MetadataMode) (io.Reader, error) {
	var op errors.Op = "metadata.apply"
	var localMetadataBytes []byte
	var err error

	localMetadataBytes, err = ioutil.ReadFile(p.ec.MetadataFile)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("reading metadata file: %w", err))
	}
	if mode == cli.MetadataModeYAML {
		localMetadataBytes, err = metadatautil.YAMLToJSON(localMetadataBytes)
		if err != nil {
			return nil, errors.E(op, fmt.Errorf("parsing yaml metadata to json: %w", err))
		}
	}
	var metadata interface{}
	err = json.Unmarshal(localMetadataBytes, &metadata)
	if err != nil {
		return nil, errors.E(op, errors.KindBadInput, fmt.Errorf("parsing metadata as json: %w", err))
	}
	if p.ec.Config.Version == cli.V2 {
		r, err := cli.GetCommonMetadataOps(p.ec).ReplaceMetadata(bytes.NewReader(localMetadataBytes))
		if err != nil {
			return nil, errors.E(op, err)
		}
		return r, nil
	}
	r, err := p.ec.APIClient.V1Metadata.V2ReplaceMetadata(hasura.V2ReplaceMetadataArgs{
		AllowInconsistentMetadata: true,
		Metadata:                  metadata,
	})
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("applying metadata: %w", err))
	}
	b := new(bytes.Buffer)
	if err := json.NewEncoder(b).Encode(r); err != nil {
		return nil, errors.E(op, fmt.Errorf("encoding json response from server: %w", err))
	}
	return b, nil
}

func export(p *ProjectMetadata, mode cli.MetadataMode) (io.Reader, error) {
	var op errors.Op = "metadata.export"
	metadata, err := p.ec.APIClient.V1Metadata.ExportMetadata()
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("exporting metadata from server: %w", err))
	}

	var metadataBytes []byte
	metadataBytes, err = ioutil.ReadAll(metadata)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("reading metadata from response: %w", err))
	}

	if mode == cli.MetadataModeYAML {
		metadataBytes, err = metadatautil.JSONToYAML(metadataBytes)
		if err != nil {
			return nil, errors.E(op, fmt.Errorf("parsing metadata to yaml: %w", err))
		}
	}

	return bytes.NewReader(metadataBytes), nil
}
