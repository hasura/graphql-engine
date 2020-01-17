package version

import (
	"fmt"
	"strings"
)

var (
	preReleaseVersion = "v1.0-alpha"
	unversioned       = "unversioned"
	versioned         = "versioned"
)

// GetConsoleTemplateVersion returns the template version tv required to render
// the console html.
func (v *Version) GetConsoleTemplateVersion() (tv string) {
	// pre-release builds
	if v.Server == "" {
		return preReleaseVersion
	}
	// tagged build
	if v.Server != "" {
		if v.ServerSemver != nil {
			return fmt.Sprintf("v%d.%d", v.ServerSemver.Major(), v.ServerSemver.Minor())
		}
	}
	// untagged version
	return unversioned
}

// GetConsoleAssetsVersion returns the assets version av to be used in the
// console template. This function is supposed to return the following:
// > input           -> output
// > dev-build       -> versioned/dev-build
// > v1.0.0-beta.01  -> beta/v1.0
// > v1.0.0-alpha.01 -> alpha/v1.0
// > v1.2.1-rc.03    -> rc/v1.2
// > v1.1.0          -> stable/v1.1
func (v *Version) GetConsoleAssetsVersion() (av string) {
	// server has a version
	if v.Server != "" {
		// version is semver
		if v.ServerSemver != nil {
			// check for release channels
			preRelease := v.ServerSemver.Prerelease()
			channel := "stable"
			if strings.HasPrefix(preRelease, "alpha") {
				channel = "alpha"
			}
			if strings.HasPrefix(preRelease, "beta") {
				channel = "beta"
			}
			if strings.HasPrefix(preRelease, "rc") {
				channel = "rc"
			}
			return fmt.Sprintf("channel/%s/v%d.%d", channel, v.ServerSemver.Major(), v.ServerSemver.Minor())
		}
		// version is not semver
		return fmt.Sprintf("%s/%s", versioned, v.Server)
	}
	// server doesn't have a version - very old server :(
	return preReleaseVersion
}
