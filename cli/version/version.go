// Package version implements server, cli and console version handling.
package version

import (
	"fmt"

	"github.com/Masterminds/semver"
)

// DevVersion is the version string for development versions.
const DevVersion = "dev"

// BuildVersion is the versin string with which CLI is built. Set during
// the build time.
var BuildVersion = DevVersion

// Version defines the version object.
type Version struct {
	// CLI is the version of CLI
	CLI string
	// CLISemver is the parsed semantic version for CLI
	CLISemver *semver.Version

	// Server is the version of server CLI is interacting with
	Server string
	// ServerSemver is the parsed semantic version for server
	ServerSemver *semver.Version

	// ServerFeatureFlags indicates what features are supported by this
	// version of server.
	ServerFeatureFlags *ServerFeatureFlags
}

// GetCLIVersion return the CLI version string.
func (v *Version) GetCLIVersion() string {
	return v.CLI
}

// GetServerVersion return the server version string.
func (v *Version) GetServerVersion() string {
	return v.Server
}

// SetCLIVersion parses the version string vs and sets it as CLI version
func (v *Version) SetCLIVersion(s string) {
	// if semver parsing fails, cv will be nil
	sv, _ := semver.NewVersion(s)
	v.CLI = s
	if sv != nil {
		v.CLI = fmt.Sprintf("v%s", sv.String())
	}
	v.CLISemver = sv
}

// SetServerVersion parses the version string vs and sets it as server version
func (v *Version) SetServerVersion(s string) {
	// if semver parsing fails, sv will be nil
	sv, _ := semver.NewVersion(s)
	v.Server = s
	if sv != nil {
		v.Server = fmt.Sprintf("v%s", sv.String())
	}
	v.ServerSemver = sv
}

// New returns a new version object with BuildVersion filled in as CLI
func New() *Version {
	v := &Version{}
	v.SetCLIVersion(BuildVersion)
	return v
}

// NewCLIVersion returns a new version object with CLI info filled in
func NewCLIVersion(cli string) *Version {
	v := &Version{}
	v.SetCLIVersion(cli)
	return v
}
