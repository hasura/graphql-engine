package metadata

import (
	"errors"
	"fmt"
	"net/url"
	"path/filepath"

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

	hasuraDBConfig *hasuraDBConfig

	metadataFiles []string
}

func New(dir string, endpoint *url.URL, adminSecret string, version *version.Version) (*config, error) {
	files := make([]string, 0)
	files = append(files, filepath.Join(dir, "metadata.yaml"))
	files = append(files, filepath.Join(dir, "metadata.json"))
	return &config{
		hasuraDBConfig: &hasuraDBConfig{
			endpoint:    endpoint,
			adminSecret: adminSecret,
			version:     version,
		},
		metadataFiles: files,
	}, nil
}

func (c *config) getMetadataFilePath(format string) (string, error) {
	ext := fmt.Sprintf(".%s", format)
	for _, filePath := range c.metadataFiles {
		switch p := filepath.Ext(filePath); p {
		case ext:
			return filePath, nil
		}
	}
	return "", errors.New("unsupported file type")
}
