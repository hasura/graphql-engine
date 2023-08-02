package metadatautil

import (
	"errors"
	"fmt"
	"io"
	"io/ioutil"

	"github.com/buger/jsonparser"
	internalerrors "github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/util"
)

func GetSourceKind(exportMetadata func() (io.Reader, error), sourceName string) (*hasura.SourceKind, error) {
	var op internalerrors.Op = "metadatautil.GetSourceKind"
	metadataReader, err := exportMetadata()
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	metadata, err := ioutil.ReadAll(metadataReader)
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	var kind *string
	_, err = jsonparser.ArrayEach(metadata, func(value []byte, dataType jsonparser.ValueType, offset int, err error) {
		var v string
		v, _ = jsonparser.GetString(value, "name")
		if v == sourceName {
			k, _ := jsonparser.GetString(value, "kind")
			if len(k) > 0 {
				kind = &k
			}
		}
	}, "sources")
	if err != nil {
		return nil, internalerrors.E(op, fmt.Errorf("jsonparser: %w", err))
	}

	if kind != nil {
		return (*hasura.SourceKind)(kind), nil
	}
	return nil, nil
}

func GetSources(exportMetadata func() (io.Reader, error)) ([]string, error) {
	var op internalerrors.Op = "metadatautil.GetSources"
	metadataReader, err := exportMetadata()
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	metadata, err := ioutil.ReadAll(metadataReader)
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	var sources []string
	_, err = jsonparser.ArrayEach(metadata, func(value []byte, dataType jsonparser.ValueType, offset int, err error) {
		v, _ := jsonparser.GetString(value, "name")
		if len(v) > 0 {
			sources = append(sources, v)
		}
	}, "sources")

	if err != nil {
		return nil, internalerrors.E(op, fmt.Errorf("jsonparser: %w", err))
	}

	return sources, nil
}

type Source struct {
	Name string            `yaml:"name"`
	Kind hasura.SourceKind `yaml:"kind"`
}

func GetSourcesAndKind(exportMetadata func() (io.Reader, error)) ([]Source, error) {
	var op internalerrors.Op = "metadatautil.GetSourcesAndKind"
	metadataReader, err := exportMetadata()
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	metadata, err := ioutil.ReadAll(metadataReader)
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	var sources []Source
	_, err = jsonparser.ArrayEach(metadata, func(value []byte, dataType jsonparser.ValueType, offset int, err error) {
		name, _ := jsonparser.GetString(value, "name")
		kind, _ := jsonparser.GetString(value, "kind")
		if len(name) > 0 && len(kind) > 0 {
			sources = append(sources, Source{
				Name: name,
				Kind: hasura.SourceKind(kind),
			})
		}
	}, "sources")
	if err != nil {
		return nil, internalerrors.E(op, fmt.Errorf("jsonparser: %w", err))
	}

	return sources, nil
}

var ErrNoConnectedSources = errors.New("0 connected sources found on hasura")

// GetSourcesAndKindStrict is like GetSourcesAndKind but will return an error when  no sources are found
func GetSourcesAndKindStrict(exportMetadata func() (io.Reader, error)) ([]Source, error) {
	var op internalerrors.Op = "metadatautil.GetSourcesAndKindStrict"
	sources, err := GetSourcesAndKind(exportMetadata)
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	if len(sources) == 0 {
		return nil, internalerrors.E(op, ErrNoConnectedSources)
	}
	return sources, nil
}

func DatabaseChooserUI(exportMetadata func() (io.Reader, error)) (string, error) {
	var op internalerrors.Op = "metadatautil.DatabaseChooserUI"
	sources, err := GetSources(exportMetadata)
	if err != nil {
		return "", internalerrors.E(op, fmt.Errorf("unable to get available databases: %w", err))
	}
	if len(sources) == 0 {
		return "", internalerrors.E(op, errors.New("no connected databases found in the server"))
	}
	databaseName, err := util.GetSelectPrompt("Select a database to use", sources)
	if err != nil {
		return "", internalerrors.E(op, fmt.Errorf("error in selecting a database to use: %w", err))
	}

	return databaseName, nil
}

const ChooseAllDatabases = "All (all available databases)"

func DatabaseChooserUIWithAll(exportMetadata func() (io.Reader, error)) (string, error) {
	var op internalerrors.Op = "metadatautil.DatabaseChooserUIWithAll"
	sources, err := GetSources(exportMetadata)
	if err != nil {
		return "", internalerrors.E(op, fmt.Errorf("unable to get available databases: %w", err))
	}
	if len(sources) == 0 {
		return "", internalerrors.E(op, errors.New("no connected databases found in the server"))
	}
	sources = append([]string{ChooseAllDatabases}, sources...)
	databaseName, err := util.GetSelectPrompt("Select a database to use", sources)
	if err != nil {
		return "", internalerrors.E(op, fmt.Errorf("error in selecting a database to use: %w", err))
	}

	return databaseName, nil
}
