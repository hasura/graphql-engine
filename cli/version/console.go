package version

import (
	"fmt"
)

var (
	preReleaseVersion = "v1.0-alpha"
	unversioned       = "unversioned"
)

// GetConsoleTemplateVersion returns the template version tv required to render
// the console html.
func (v *Version) GetConsoleTemplateVersion() (tv string) {
	// pre-release builds
	if v.Server == "" {
		return preReleaseVersion
	}
	// untagged build
	if v.Server != "" {
		if v.ServerSemver != nil {
			return fmt.Sprintf("v%d.%d", v.ServerSemver.Major(), v.ServerSemver.Minor())
		}
	}
	return unversioned
}

// GetConsoleAssetsVersion returns the assets version av to be used in the
// console template.
func (v *Version) GetConsoleAssetsVersion() (av string) {
	if v.Server != "" {
		if v.ServerSemver != nil {
			return fmt.Sprintf("v%d.%d", v.ServerSemver.Major(), v.ServerSemver.Minor())
		}
		return v.Server
	}
	return preReleaseVersion
}
