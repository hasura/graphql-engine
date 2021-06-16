package metadatautil

import (
	"errors"
	"fmt"
	"io"
	"io/ioutil"

	"github.com/goccy/go-yaml"
	"github.com/goccy/go-yaml/parser"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/util"
)

func getMetadataAsYaml(exportMetadata func() (io.Reader, error)) ([]byte, error) {
	metadata, err := exportMetadata()
	if err != nil {
		return nil, err
	}
	jsonb, err := ioutil.ReadAll(metadata)
	if err != nil {
		return nil, err
	}
	yamlb, err := yaml.JSONToYAML(jsonb)
	if err != nil {
		return nil, err
	}
	return yamlb, err
}

func GetSourceKind(exportMetadata func() (io.Reader, error), sourceName string) (*hasura.SourceKind, error) {
	metadata, err := getMetadataAsYaml(exportMetadata)
	if err != nil {
		return nil, err
	}
	ast, err := parser.ParseBytes(metadata, 0)
	if err != nil {
		return nil, err
	}
	if len(ast.Docs) <= 0 {
		return nil, fmt.Errorf("failed listing sources from metadata")
	}
	var sources []struct {
		Name string            `yaml:"name"`
		Kind hasura.SourceKind `yaml: "kind"`
	}
	path, err := yaml.PathString("$.sources[*]")
	if err != nil {
		return nil, err
	}
	if err := path.Read(ast.Docs[0], &sources); err != nil {
		return nil, err
	}
	for _, s := range sources {
		if s.Name == sourceName {
			return &s.Kind, nil
		}
	}
	return nil, nil
}

func GetSources(exportMetadata func() (io.Reader, error)) ([]string, error) {
	metadata, err := getMetadataAsYaml(exportMetadata)
	if err != nil {
		return nil, err
	}
	ast, err := parser.ParseBytes(metadata, 0)
	if err != nil {
		return nil, err
	}
	if len(ast.Docs) <= 0 {
		return nil, fmt.Errorf("failed listing sources from metadata")
	}
	var sources []string
	path, err := yaml.PathString("$.sources[*].name")
	if err != nil {
		return nil, err
	}
	if err := path.Read(ast.Docs[0], &sources); err != nil {
		return nil, err
	}
	return sources, nil
}

type Source struct {
	Name string            `yaml: "name"`
	Kind hasura.SourceKind `yaml:"kind"`
}

func GetSourcesAndKind(exportMetadata func() (io.Reader, error)) ([]Source, error) {
	metadata, err := getMetadataAsYaml(exportMetadata)
	if err != nil {
		return nil, err
	}
	ast, err := parser.ParseBytes(metadata, 0)
	if err != nil {
		return nil, err
	}
	if len(ast.Docs) <= 0 {
		return nil, fmt.Errorf("failed listing sources from metadata")
	}
	path, err := yaml.PathString("$.sources")
	if err != nil {
		return nil, err
	}
	var sources []Source
	if err := path.Read(ast.Docs[0], &sources); err != nil {
		return nil, err
	}
	return sources, nil
}

func DatabaseChooserUI(exportMetadata func() (io.Reader, error)) (string, error) {
	sources, err := GetSources(exportMetadata)
	if err != nil {
		return "", fmt.Errorf("unable to get available databases: %w", err)
	}
	if len(sources) == 0 {
		return "", errors.New("no connected databases found in the server")
	}
	databaseName, err := util.GetSelectPrompt("Select a database to use", sources)
	if err != nil {
		return "", fmt.Errorf("error in selecting a database to use: %w", err)
	}

	return databaseName, nil
}
