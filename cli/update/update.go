package update

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"

	"github.com/kardianos/osext"
)

const (
	AppRepoOwner string = "hasura"
	AppName      string = "graphql-engine"
)

func CheckUpdate(version string) (bool, *Release, *Asset, error) {
	releases, err := getLatestRelease(AppRepoOwner, AppName)
	if err != nil {
		return false, nil, nil, err
	}

	if len(releases) == 0 {
		return false, nil, nil, nil
	}

	releaseInfo := releases[0]

	if releaseInfo.TagName != version {
		assetToDownload := releaseInfo.GetAsset(buildFilename())
		if assetToDownload == nil {
			return false, releaseInfo, nil, nil
		}
		return true, releaseInfo, assetToDownload, nil
	}
	return false, releaseInfo, nil, nil
}

func ApplyUpdate(asset *Asset) error {
	currentExecutable, err := osext.Executable()
	if err != nil {
		return err
	}

	exPath := filepath.Dir(currentExecutable)

	file, err := asset.DownloadBinary(buildFilename(), exPath)
	if err != nil {
		return err
	}

	err = os.Rename(file.Name(), currentExecutable)
	if err != nil {
		return err
	}
	return nil
}

func buildFilename() string {
	extension := ""
	if runtime.GOOS == "windows" {
		extension = ".exe"
	}
	return fmt.Sprintf("cli-%s-%s-%s%s", AppRepoOwner, runtime.GOOS, runtime.GOARCH, extension)
}
