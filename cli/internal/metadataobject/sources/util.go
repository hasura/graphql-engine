package sources

import (
	"fmt"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"go.yaml.in/yaml/v3"
)

// mappingValue returns the value of key in a YAML mapping node, or nil.
func mappingValue(node *yaml.Node, key string) *yaml.Node {
	if node == nil || node.Kind != yaml.MappingNode {
		return nil
	}

	for i := 0; i+1 < len(node.Content); i += 2 {
		if node.Content[i].Value == key {
			return node.Content[i+1]
		}
	}

	return nil
}

// isLiteralString reports whether node is a non-empty string value.
func isLiteralString(node *yaml.Node) bool {
	return node != nil &&
		node.Kind == yaml.ScalarNode &&
		node.ShortTag() == "!!str" &&
		node.Value != ""
}

// plainTextCredentials returns the fields of a source configuration that hold
// credentials as literal values instead of {from_env: ...}, so they end up in
// the project files. It covers the connection info of Postgres like sources and
// MS SQL Server, including read replicas and connection sets, and the service
// account of BigQuery. A database_url with {{ENV_VAR}} templates is assumed to
// read its credentials from the environment.
func plainTextCredentials(configuration yaml.Node) []string {
	var fields []string

	checkConnectionInfo := func(prefix string, connectionInfo *yaml.Node) {
		databaseURL := mappingValue(connectionInfo, "database_url")
		connectionParameters := mappingValue(databaseURL, "connection_parameters")

		switch {
		case isLiteralString(databaseURL):
			if !strings.Contains(databaseURL.Value, "{{") {
				fields = append(fields, prefix+"database_url")
			}
		case isLiteralString(mappingValue(connectionParameters, "password")):
			fields = append(fields, prefix+"database_url.connection_parameters.password")
		}

		if isLiteralString(mappingValue(connectionInfo, "connection_string")) {
			fields = append(fields, prefix+"connection_string")
		}
	}

	checkConnectionInfo("connection_info.", mappingValue(&configuration, "connection_info"))

	for idx, replica := range sequenceItems(mappingValue(&configuration, "read_replicas")) {
		checkConnectionInfo(fmt.Sprintf("read_replicas[%d].", idx), replica)
	}

	for idx, member := range sequenceItems(mappingValue(&configuration, "connection_set")) {
		checkConnectionInfo(
			fmt.Sprintf("connection_set[%d].connection_info.", idx),
			mappingValue(member, "connection_info"),
		)
	}

	serviceAccount := mappingValue(&configuration, "service_account")
	if isLiteralString(serviceAccount) ||
		(serviceAccount != nil && serviceAccount.Kind == yaml.MappingNode &&
			mappingValue(serviceAccount, "from_env") == nil) {
		fields = append(fields, "service_account")
	}

	return fields
}

// sequenceItems returns the items of a YAML sequence node, or nil.
func sequenceItems(node *yaml.Node) []*yaml.Node {
	if node == nil || node.Kind != yaml.SequenceNode {
		return nil
	}

	return node.Content
}

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
	var (
		op   errors.Op = "sources.fileNameWhenTableIsObject"
		node struct {
			Name string `yaml:"name"`
			// Depending on the datasource the namespacing parameter can change.
			// for example in pg and mssql the namespacing is done with the concept of schemas
			// and in cases like bigquery it'll be called "dataset"
			Schema  string `yaml:"schema"`
			Dataset string `yaml:"dataset"`
		}
	)

	yamlString, _ := yaml.Marshal(yamlNode)

	err := yaml.Unmarshal(yamlString, &node)
	if err != nil {
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

	err := yaml.Unmarshal(yamlString, &strArr)
	if err != nil {
		return "", errors.E(op, err)
	}

	fileName := strings.Join(
		strArr,
		"_",
	) + ".yaml" // Concat all elements in the array delimited with "_", and append .yaml at the end

	return fileName, nil
}
