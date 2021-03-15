package v3

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/Masterminds/semver"
)

const (
	metadataDirPrefixV3 = "v3/metadata"
)

func getMetadataDir(serverVersion *semver.Version) string {
	var version string
	if serverVersion == nil {
		version = "latest"
	} else {
		currDir, _ := os.Getwd()
		versionDir := fmt.Sprintf("v%d.%d", serverVersion.Major(), serverVersion.Minor())
		if _, err := os.Stat(filepath.Join(currDir, metadataDirPrefixV3, versionDir)); err != nil {
			version = "latest"
		} else {
			version = versionDir
		}
	}
	return filepath.Join(metadataDirPrefixV3, version)
}
