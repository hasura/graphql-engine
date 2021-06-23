package commands

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/util"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v2"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

const (
	defaultConfigFilename = "config.yaml"
)

func TestE2e(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "e2e testsuite")
}

// EditEndpoint in config
func editEndpointInConfig(configFilePath, endpoint string) {
	var config cli.Config
	b, err := ioutil.ReadFile(configFilePath)
	Expect(err).ShouldNot(HaveOccurred())

	err = yaml.Unmarshal(b, &config)
	Expect(err).ShouldNot(HaveOccurred())

	config.Endpoint = endpoint

	b, err = yaml.Marshal(&config)
	Expect(err).ShouldNot(HaveOccurred())

	err = ioutil.WriteFile(configFilePath, b, 0655)
	Expect(err).ShouldNot(HaveOccurred())

}

func editSourceNameInConfigV3ProjectTemplate(projectDir, sourceName, postgresConnectionString string) {
	// assumes it's renaming a copy of commands/testdata/config-v3-test-project
	Expect(os.Rename(filepath.Join(projectDir, "migrations", "pg"), filepath.Join(projectDir, "migrations", sourceName))).To(BeNil())
	Expect(os.Rename(filepath.Join(projectDir, "seeds", "pg"), filepath.Join(projectDir, "seeds", sourceName))).To(BeNil())

	databaseYamlFilename := filepath.Join(projectDir, "metadata", "databases", "databases.yaml")
	databasesYaml, err := ioutil.ReadFile(databaseYamlFilename)
	Expect(err).To(BeNil())
	newDatabasesYaml := strings.Replace(string(databasesYaml), "pg", fmt.Sprintf("%v", sourceName), -1)
	newDatabasesYaml = strings.Replace(string(newDatabasesYaml), "database_url: TO_BE_FILLED", fmt.Sprintf("database_url: %v", postgresConnectionString), -1)
	Expect(ioutil.WriteFile(databaseYamlFilename, []byte(newDatabasesYaml), 0655)).To(BeNil())

	Expect(os.Rename(filepath.Join(projectDir, "metadata", "databases", "pg"), filepath.Join(projectDir, "metadata", "databases", sourceName))).To(BeNil())
}

func copyTestConfigV2Project(dest string) {
	p, err := filepath.Abs("testdata/config-v2-test-project")
	Expect(err).To(BeNil())
	Expect(util.CopyDir(p, dest)).To(BeNil())
}

func copyTestConfigV3Project(dest string) {
	p, err := filepath.Abs("testdata/config-v3-test-project")
	Expect(err).To(BeNil())
	Expect(util.CopyDir(p, dest)).To(BeNil())
}
