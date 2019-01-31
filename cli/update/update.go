package update

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"path/filepath"
	"runtime"

	"github.com/kardianos/osext"

	"github.com/Masterminds/semver"
	"github.com/pkg/errors"
)

const updateCheckURL = "https://releases.hasura.io/graphql-engine?agent=cli"

type updateCheckResponse struct {
	Latest string `json:"latest"`
}

func getLatestVersion() (*semver.Version, error) {
	res, err := http.Get(updateCheckURL)
	if err != nil {
		return nil, errors.Wrap(err, "update check request")
	}

	defer res.Body.Close()
	var response updateCheckResponse
	err = json.NewDecoder(res.Body).Decode(&response)
	if err != nil {
		return nil, errors.Wrap(err, "decoding update check response")
	}

	v, err := semver.NewVersion(response.Latest)
	if err != nil {
		return nil, errors.Wrap(err, "semver parsing")
	}

	return v, nil
}

func buildAssetURL(v string) string {
	os := runtime.GOOS
	arch := runtime.GOARCH
	extension := ""
	if os == "windows" {
		extension = ".exe"
	}
	return fmt.Sprintf(
		"https://github.com/hasura/graphql-engine/releases/download/v%s/cli-hasura-%s-%s%s",
		v, os, arch, extension,
	)
}

func downloadAsset(url, fileName, filePath string) (*os.File, error) {
	res, err := http.Get(url)
	if err != nil {
		return nil, errors.Wrap(err, "downloading asset")
	}
	defer res.Body.Close()

	if res.StatusCode != 200 {
		return nil, errors.New("could not find the release asset")
	}

	asset, err := ioutil.TempFile(filePath, fileName)
	if err != nil {
		return nil, errors.Wrap(err, "creating temporary file")
	}
	defer asset.Close()

	_, err = io.Copy(asset, res.Body)
	if err != nil {
		return nil, errors.Wrap(err, "saving downloaded file")
	}

	err = asset.Chmod(0755)
	if err != nil {
		return nil, errors.Wrap(err, "changing downloaded file permissions")
	}

	return asset, nil
}

// HasUpdate tells us if there is a new update available.
func HasUpdate(currentVersion *semver.Version) (bool, *semver.Version, error) {
	latestVersion, err := getLatestVersion()
	if err != nil {
		return false, nil, errors.Wrap(err, "get latest version")
	}

	c, err := semver.NewConstraint(fmt.Sprintf("> %s", currentVersion.String()))
	if err != nil {
		return false, nil, errors.Wrap(err, "semver constraint build")
	}

	return c.Check(latestVersion), latestVersion, nil
}

// ApplyUpdate downloads and applies the update.
func ApplyUpdate(v *semver.Version) error {
	exe, err := osext.Executable()
	if err != nil {
		return errors.Wrap(err, "find executable")
	}

	exePath := filepath.Dir(exe)
	exeName := filepath.Base(exe)

	asset, err := downloadAsset(
		buildAssetURL(v.String()), exeName+".new", exePath,
	)
	if err != nil {
		return errors.Wrap(err, "download asset")
	}

	err = os.Rename(asset.Name(), exe)
	if err != nil {
		return errors.Wrap(err, "rename asset")
	}

	return nil
}
