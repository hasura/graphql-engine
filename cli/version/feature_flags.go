package version

import (
	"github.com/Masterminds/semver"
	"github.com/pkg/errors"
)

// ServerFeatureFlags indicates what features are supported by this
// version of server.
type ServerFeatureFlags struct {
	HasAccessKey bool
}

const adminSecretVersion = "v1.0.0-alpha38"

// GetServerFeatureFlags returns the feature flags for server.
func (v *Version) GetServerFeatureFlags() (*ServerFeatureFlags, error) {
	flags := &ServerFeatureFlags{}

	// create a constraint to check if the current server version has admin secret
	adminSecretConstraint, err := semver.NewConstraint("< " + adminSecretVersion)
	if err != nil {
		return nil, errors.Wrap(err, "building admin secret constraint failed")
	}
	// check the current version with the constraint
	flags.HasAccessKey = adminSecretConstraint.Check(v.ServerSemver)

	return flags, nil
}
