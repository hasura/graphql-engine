package sources

import (
	"fmt"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"gopkg.in/yaml.v3"
)

func getTableYamlFileName(node yaml.Node) (string, error) {
	var op errors.Op = "sources.getTableYamlFileName"
	if node.Kind == yaml.SequenceNode {
		filename, err := fileNameWhenTableIsArray(node)
		if err != nil {
			return "", errors.E(op, err)
		}
		return filename, nil
	}
	filename, err := fileNameWhenTableIsObject(node)
	if err != nil {
		return "", errors.E(op, err)
	}
	return filename, nil
}

func fileNameWhenTableIsObject(yamlNode yaml.Node) (string, error) {
	var op errors.Op = "sources.fileNameWhenTableIsObject"
	var node struct {
		Name string `yaml:"name"`
		// Depending on the datasource the namespacing parameter can change.
		// for example in pg and mssql the namespacing is done with the concept of schemas
		// and in cases like bigquery it'll be called "dataset"
		Schema  string `yaml:"schema"`
		Dataset string `yaml:"dataset"`
	}
	yamlString, _ := yaml.Marshal(yamlNode)
	if err := yaml.Unmarshal(yamlString, &node); err != nil {
		return "", errors.E(op, err)
	}
	if node.Schema == "" {
		return fmt.Sprintf("%s_%s.yaml", node.Dataset, node.Name), nil
	}
	return fmt.Sprintf("%s_%s.yaml", node.Schema, node.Name), nil
}

func fileNameWhenTableIsArray(yamlNode yaml.Node) (string, error) {
	var op errors.Op = "sources.fileNameWhenTableIsArray"
	var strArr []string // var that stores individual elememts as strings
	yamlString, _ := yaml.Marshal(yamlNode)
	if err := yaml.Unmarshal(yamlString, &strArr); err != nil {
		return "", errors.E(op, err)
	}
	fileName := fmt.Sprintf("%s.yaml", strings.Join(strArr, "_")) // Concat all elements in the array delimited with "_", and append .yaml at the end
	return fileName, nil
}
