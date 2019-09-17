package util

import (
	"fmt"
	"net/url"
	"runtime"
	"strings"

	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/hasura/graphql-engine/cli/version"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

func NewMigrate(dir string, db *url.URL, adminSecretValue string, logger *logrus.Logger, v *version.Version) (*migrate.Migrate, error) {
	dbURL := GetDataPath(db, GetAdminSecretHeaderName(v), adminSecretValue)
	fileURL := GetFilePath(dir)
	t, err := migrate.New(fileURL.String(), dbURL.String(), true, logger)
	if err != nil {
		return nil, errors.Wrap(err, "cannot create migrate instance")
	}
	return t, nil
}

func GetDataPath(nurl *url.URL, adminSecretHeader, adminSecretValue string) *url.URL {
	host := &url.URL{
		Scheme: "hasuradb",
		Host:   nurl.Host,
		Path:   nurl.Path,
	}
	q := nurl.Query()
	// Set sslmode in query
	switch scheme := nurl.Scheme; scheme {
	case "https":
		q.Set("sslmode", "enable")
	default:
		q.Set("sslmode", "disable")
	}
	if adminSecretValue != "" {
		q.Add("headers", fmt.Sprintf("%s:%s", adminSecretHeader, adminSecretValue))
	}
	host.RawQuery = q.Encode()
	return host
}

func GetFilePath(dir string) *url.URL {
	host := &url.URL{
		Scheme: "file",
		Path:   dir,
	}

	// Add Prefix / to path if runtime.GOOS equals to windows
	if runtime.GOOS == "windows" && !strings.HasPrefix(host.Path, "/") {
		host.Path = "/" + host.Path
	}
	return host
}
