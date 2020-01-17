package version

const (
	untaggedBuild   = "untagged build, there could be inconsistencies"
	taggedBuild     = "older cli version might not be compatible with latest server apis, please update cli"
	noServerVersion = "server with no version treated as pre-release build"
	noCLIVersion    = "cli version is empty, indicates a broken build"
	untaggedCLI     = "untagged cli build can work with tagged server build"
	devCLI          = "dev version of cli, there could be inconsistencies"
)

// CheckCLIServerCompatibility compares server and cli for compatibility,
// subject to certain conditions. compatible boolean is returned along with
// a message which states the reason for the result.
func (v *Version) CheckCLIServerCompatibility() (compatible bool, reason string) {
	// mark dev builds as compatible
	if v.CLI == DevVersion {
		return true, devCLI
	}
	// empty cli version
	if v.CLI == "" {
		return false, noCLIVersion
	}

	// empty server version
	if v.Server == "" {
		return true, noServerVersion
	}

	// untagged server version
	if v.Server != "" && v.ServerSemver == nil {
		// same cli version
		if v.CLI == v.Server {
			return true, untaggedBuild
		}
		// different cli version
		return false, untaggedBuild

	}

	// tagged server version
	if v.Server != "" && v.ServerSemver != nil {
		// cli is also tagged build
		if v.CLI != "" && v.CLISemver != nil {
			if (v.CLISemver.Major() >= v.ServerSemver.Major()) && (v.CLISemver.Minor() >= v.ServerSemver.Minor()) {
				return true, taggedBuild
			}
			return false, taggedBuild
		}
	}

	// untagged cli version
	if v.CLI != "" && v.CLISemver == nil {
		return true, untaggedCLI
	}

	return false, "versions are incompatible"
}
