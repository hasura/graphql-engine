package commands

import (
	"io/ioutil"
	"testing"

	"github.com/hasura/graphql-engine/cli"
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
