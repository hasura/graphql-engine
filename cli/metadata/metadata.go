package metadata

import (
	"net/url"
	"os"

	"github.com/hasura/graphql-engine/cli/version"
	"github.com/sirupsen/logrus"
)

type hasuraDBConfig struct {
	endpoint *url.URL

	adminSecret string

	version *version.Version
}

type config struct {
	Logger *logrus.Logger

	sourceDir string

	metadataPath string

	hasuraDBConfig *hasuraDBConfig
}

func New(dir string, endpoint *url.URL, adminSecret string, version *version.Version) (*config, error) {
	return &config{
		hasuraDBConfig: &hasuraDBConfig{
			endpoint:    endpoint,
			adminSecret: adminSecret,
			version:     version,
		},
	}, nil
}

func (c *config) SetMetadataPath(path string, checkExists bool) error {
	// check if file exists
	if checkExists {
		_, err := os.Stat(path)
		if err != nil {
			return err
		}
	}
	c.metadataPath = path
	return nil
}
