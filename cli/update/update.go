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

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

const updateCheckURL = "https://releases.hasura.io/graphql-engine?agent=cli"

type updateCheckResponse struct {
	Latest     *semver.Version `json:"latest"`
	PreRelease *semver.Version `json:"prerelease"`
}

func getLatestVersion() (*semver.Version, *semver.Version, error) {
	var op errors.Op = "update.getLatestVersion"
	res, err := http.Get(updateCheckURL)
	if err != nil {
		return nil, nil, errors.E(op, fmt.Errorf("update check request: %w", err))
	}

	defer res.Body.Close()
	var response updateCheckResponse
	err = json.NewDecoder(res.Body).Decode(&response)
	if err != nil {
		return nil, nil, errors.E(op, fmt.Errorf("decoding update check response: %w", err))
	}
	if response.Latest == nil && response.PreRelease == nil {
		return nil, nil, errors.E(op, fmt.Errorf("expected version info not found at %s", updateCheckURL))
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

func downloadAsset(url, fileName, filePath string) (*os.File, error) {
	var op errors.Op = "update.downloadAsset"
	res, err := http.Get(url)
	if err != nil {
		return nil, errors.E(op, errors.KindNetwork, fmt.Errorf("downloading asset: %w", err))
	}
	defer res.Body.Close()

	if res.StatusCode != 200 {
		return nil, errors.E(op, errors.E("could not find the release asset"))
	}

	asset, err := os.OpenFile(
		filepath.Join(filePath, fileName),
		os.O_CREATE|os.O_WRONLY|os.O_TRUNC,
		0755,
	)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("creating new binary file: %w", err))
	}
	defer asset.Close()

	_, err = io.Copy(asset, res.Body)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("saving downloaded file: %w", err))
	}

	return asset, nil
}

// HasUpdate tells us if there is a new stable or prerelease update available.
func HasUpdate(currentVersion *semver.Version, timeFile string) (bool, *semver.Version, bool, *semver.Version, error) {
	var op errors.Op = "update.HasUpdate"
	if timeFile != "" {
		defer func() {
			if err := writeTimeToFile(timeFile, time.Now().UTC()); err != nil {
				fmt.Fprintln(os.Stderr, "failed writing last update check time: ", err)
			}
		}()
	}

	latestVersion, preReleaseVersion, err := getLatestVersion()
	if err != nil {
		return false, nil, false, nil, errors.E(op, fmt.Errorf("get latest version: %w", err))
	}

	return latestVersion.GreaterThan(currentVersion), latestVersion, preReleaseVersion.GreaterThan(currentVersion), preReleaseVersion, nil
}

// ApplyUpdate downloads and applies the update indicated by version v.
func ApplyUpdate(v *semver.Version) error {
	var op errors.Op = "update.ApplyUpdate"
	// get the current executable
	exe, err := osext.Executable()
	if err != nil {
		return errors.E(op, fmt.Errorf("find executable: %w", err))
	}

	// extract the filename and path
	exePath := filepath.Dir(exe)
	exeName := filepath.Base(exe)

	// download the new binary
	asset, err := downloadAsset(
		buildAssetURL(v.String()), "."+exeName+".new", exePath,
	)
	if err != nil {
		return errors.E(op, fmt.Errorf("download asset: %w", err))
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
		return errors.E(op, fmt.Errorf("rename exe to old: %w", err))
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
			return errors.E(op, fmt.Errorf(
				"rename old to exe: inconsistent state, re-install cli: %w",
				rerr))
		}
		// rolled back, throw update error
		return errors.E(op, fmt.Errorf("rename new to exe: %w", err))
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
