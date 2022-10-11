package commands

import (
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"net/url"
	"os"
	"path"
	"path/filepath"
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"

	"github.com/hasura/graphql-engine/cli/v2/util"

	"github.com/hasura/graphql-engine/cli/v2"
	"gopkg.in/yaml.v3"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

const (
	defaultConfigFilename = "config.yaml"
	timeout               = testutil.DefaultE2ETestTimeout
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

func editMetadataFileInConfig(configFilePath, path string) {
	var config cli.Config
	b, err := ioutil.ReadFile(configFilePath)
	Expect(err).ShouldNot(HaveOccurred())

	err = yaml.Unmarshal(b, &config)
	Expect(err).ShouldNot(HaveOccurred())

	config.MetadataFile = path

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

func copyMigrationsToProjectDirectory(projectDirectory, migrationsDirectory string, sources ...string) {
	projectMigrationsDirectory := filepath.Join(projectDirectory, "migrations")
	if len(sources) == 0 {
		// should be a config v2 project
		Expect(os.RemoveAll(projectMigrationsDirectory)).To(BeNil())
		Expect(util.CopyDir(migrationsDirectory, filepath.Join(projectDirectory, "migrations"))).To(BeNil())
	}
	for _, source := range sources {
		// remove existing migrations from project directory
		Expect(os.RemoveAll(projectMigrationsDirectory)).To(BeNil())
		// move new migrations
		Expect(util.CopyDir(migrationsDirectory, filepath.Join(projectMigrationsDirectory, source))).To(BeNil())
	}
}

var assertHGEAPIRequestSucceedsAndGetResponseBody = func(hgeEndpoint string, urlPath string, body io.Reader) []byte {
	uri, err := url.Parse(hgeEndpoint)
	Expect(err).To(BeNil())
	uri.Path = path.Join(uri.Path, urlPath)
	req, err := http.NewRequest("POST", uri.String(), body)
	Expect(err).To(BeNil())

	req.Header.Set("Content-Type", "application/json")
	adminSecret := os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET")
	if adminSecret != "" {
		req.Header.Set("x-hasura-admin-secret", adminSecret)
	}

	resp, err := http.DefaultClient.Do(req)
	Expect(err).To(BeNil())
	defer resp.Body.Close()
	Expect(fmt.Sprint(resp.StatusCode)).Should(ContainSubstring(fmt.Sprint(http.StatusOK)))
	responseBody, err := ioutil.ReadAll(resp.Body)
	Expect(err).To(BeNil())
	return responseBody
}
