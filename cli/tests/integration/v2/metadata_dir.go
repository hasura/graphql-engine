package v2

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/Masterminds/semver"
)

const (
	metadataDirPrefix = "v2/metadata"
)

func getMetadataDir(serverVersion *semver.Version) string {
	var version string
	if serverVersion == nil {
		version = "latest"
	} else {
		currDir, _ := os.Getwd()
		versionDir := fmt.Sprintf("v%d.%d", serverVersion.Major(), serverVersion.Minor())
		if _, err := os.Stat(filepath.Join(currDir, metadataDirPrefix, versionDir)); err != nil {
			version = "latest"
		} else {
			version = versionDir
		}
	}
	return filepath.Join(metadataDirPrefix, version)
}
