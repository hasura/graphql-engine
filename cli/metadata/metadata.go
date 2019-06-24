package metadata

import (
	"net/url"
	"os"

	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/hasura/graphql-engine/cli/util"
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

	migrateDrv *migrate.Migrate
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

func (c *config) createMigrateInstance() error {
	migrateDrv, err := util.NewMigrate(c.sourceDir, c.hasuraDBConfig.endpoint, c.hasuraDBConfig.adminSecret, c.Logger, c.hasuraDBConfig.version)
	if err != nil {
		return err
	}
	c.migrateDrv = migrateDrv
	return nil
}
