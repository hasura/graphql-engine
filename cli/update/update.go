package update

// adapted from
// https://github.com/inconshreveable/go-update

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"time"

	"github.com/kardianos/osext"

	"github.com/Masterminds/semver"
	"github.com/pkg/errors"
)

const updateCheckURL = "https://releases.hasura.io/graphql-engine?agent=cli"

type updateCheckResponse struct {
	Latest     *semver.Version `json:"latest"`
	PreRelease *semver.Version `json:"prerelease"`
}

func getLatestVersion(client *http.Client) (*semver.Version, *semver.Version, error) {
	res, err := client.Get(updateCheckURL)
	if err != nil {
		return nil, nil, errors.Wrap(err, "update check request")
	}

	defer res.Body.Close()
	var response updateCheckResponse
	err = json.NewDecoder(res.Body).Decode(&response)
	if err != nil {
		return nil, nil, errors.Wrap(err, "decoding update check response")
	}

	return response.Latest, response.PreRelease, nil
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

func downloadAsset(client *http.Client, url, fileName, filePath string) (*os.File, error) {
	res, err := client.Get(url)
	if err != nil {
		return nil, errors.Wrap(err, "downloading asset")
	}
	defer res.Body.Close()

	if res.StatusCode != 200 {
		return nil, errors.New("could not find the release asset")
	}

	asset, err := os.OpenFile(
		filepath.Join(filePath, fileName),
		os.O_CREATE|os.O_WRONLY|os.O_TRUNC,
		0755,
	)
	if err != nil {
		return nil, errors.Wrap(err, "creating new binary file")
	}
	defer asset.Close()

	_, err = io.Copy(asset, res.Body)
	if err != nil {
		return nil, errors.Wrap(err, "saving downloaded file")
	}

	return asset, nil
}

// HasUpdate tells us if there is a new stable or prerelease update available.
func HasUpdate(client *http.Client, currentVersion *semver.Version, timeFile string) (bool, *semver.Version, bool, *semver.Version, error) {
	if timeFile != "" {
		defer writeTimeToFile(timeFile, time.Now().UTC())
	}

	latestVersion, preReleaseVersion, err := getLatestVersion(client)
	if err != nil {
		return false, nil, false, nil, errors.Wrap(err, "get latest version")
	}

	return latestVersion.GreaterThan(currentVersion), latestVersion, preReleaseVersion.GreaterThan(currentVersion), preReleaseVersion, nil
}

// ApplyUpdate downloads and applies the update indicated by version v.
func ApplyUpdate(client *http.Client, v *semver.Version) error {
	// get the current executable
	exe, err := osext.Executable()
	if err != nil {
		return errors.Wrap(err, "find executable")
	}

	// extract the filename and path
	exePath := filepath.Dir(exe)
	exeName := filepath.Base(exe)

	// download the new binary
	asset, err := downloadAsset(
		client,
		buildAssetURL(v.String()), "."+exeName+".new", exePath,
	)
	if err != nil {
		return errors.Wrap(err, "download asset")
	}

	// get the downloaded binary name and build the absolute path
	newExe := asset.Name()

	// build name and absolute path for saving old binary
	oldExeName := "." + exeName + ".old"
	oldExe := filepath.Join(exePath, oldExeName)

	// delete any existing old binary file - this is necessary on Windows for two reasons:
	// 1. after a successful update, Windows can't remove the .old file because the process is still running
	// 2. windows rename operations fail if the destination file already exists
	_ = os.Remove(oldExe)

	// rename the current binary as old binary
	err = os.Rename(exe, oldExe)
	if err != nil {
		return errors.Wrap(err, "rename exe to old")
	}

	// rename the new binary as the current binary
	err = os.Rename(newExe, exe)
	if err != nil {
		// rename unsuccessfull
		//
		// The filesystem is now in a bad state. We have successfully
		// moved the existing binary to a new location, but we couldn't move the new
		// binary to take its place. That means there is no file where the current executable binary
		// used to be!
		// Try to rollback by restoring the old binary to its original path.
		rerr := os.Rename(oldExe, exe)
		if rerr != nil {
			// rolling back failed, ask user to re-install cli
			return errors.Wrap(
				rerr,
				"rename old to exe: inconsistent state, re-install cli",
			)
		}
		// rolled back, throw update error
		return errors.Wrap(err, "rename new to exe")
	}

	// rename success, remove the old binary
	errRemove := os.Remove(oldExe)

	// windows has trouble removing old binaries, so hide it instead
	// it will be removed next time this code runs.
	if errRemove != nil {
		_ = hideFile(oldExe)
	}

	return nil
}
