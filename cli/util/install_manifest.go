package util

import (
	"fmt"
	"io/ioutil"
	"net/url"
	"os"
	"path/filepath"
	"strconv"
	"time"

	"github.com/pkg/errors"
)

// InstallManifestsRepo is the GitHub repo where GraphQL Engine installation
// manifests are kept.
type InstallManifestsRepo struct {
	// Namespace of the repo (e.g. hasura from github.com/hasura).
	Namespace string
	// Name of the repo (e.g. graphql-engine-install-manifests from
	// github.com/hasura/graphql-engine-install-manifests).
	Name string
}

// ZipURL returns the URL to download the repo from GitHub in ZIP format.
func (i *InstallManifestsRepo) ZipURL() string {
	u := url.URL{
		Scheme: "https",
		Host:   "github.com",
		Path:   fmt.Sprintf("%s/%s/archive/master.zip", i.Namespace, i.Name),
	}
	return u.String()
}

// Download downloads the repo contents into a temporary directory and returns
// the directory path. It is the caller's responsibility to remove the directory
// after handling the contents.
func (i *InstallManifestsRepo) Download() (dir string, err error) {
	target, err := ioutil.TempDir("", "h-gql-temp")
	if err != nil {
		return "", errors.Wrap(err, "failed to create a temp dir")
	}
	err = Download(i.ZipURL(), filepath.Join(target, "manifests.zip"))
	if err != nil {
		return "", errors.Wrap(err, "failed downloading manifests")
	}
	err = Unzip(filepath.Join(target, "manifests.zip"),
		filepath.Join(target, "manifests"))
	if err != nil {
		return "", errors.Wrap(err, "failed extracting manifests")
	}
	timestamp := strconv.Itoa(int(time.Now().Unix()))
	dir = filepath.Join(os.TempDir(), "hasuragraphqlinstall"+timestamp)
	if err != nil {
		return "", errors.Wrap(err, "failed creating temp dir")
	}
	err = CopyDir(filepath.Join(target, "manifests", i.ZipExtractedDirectory()), dir)
	if err != nil {
		return "", errors.Wrap(err, "failed copying files to temp dir")
	}
	os.RemoveAll(target)
	return dir, nil
}

// ZipExtractedDirectory returns the name of directory obtained after extracting
// the ZIP file downloaded from GitHub.
func (i *InstallManifestsRepo) ZipExtractedDirectory() string {
	return fmt.Sprintf("%s-master", i.Name)
}

// ReadmeURL returns the GitHub URL to README.md file in the repo root.
func (i *InstallManifestsRepo) ReadmeURL() string {
	u := url.URL{
		Scheme: "https",
		Host:   "github.com",
		Path:   fmt.Sprintf("%s/%s/blob/master/README.md", i.Namespace, i.Name),
	}
	return u.String()
}
