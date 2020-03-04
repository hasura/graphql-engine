package version

import (
	"github.com/Masterminds/semver"
	"github.com/pkg/errors"
)

// ServerFeatureFlags indicates what features are supported by this
// version of server.
type ServerFeatureFlags struct {
	HasAccessKey bool

	HasAction bool
}

const adminSecretVersion = "v1.0.0-alpha38"
const actionVersion = "v1.2.0-beta.1"

// GetServerFeatureFlags returns the feature flags for server.
func (v *Version) GetServerFeatureFlags() error {
	flags := &ServerFeatureFlags{}
	if v.ServerSemver == nil {
		flags.HasAccessKey = false
		flags.HasAction = true
	} else {
		// create a constraint to check if the current server version has admin secret
		adminSecretConstraint, err := semver.NewConstraint("< " + adminSecretVersion)
		if err != nil {
			return errors.Wrap(err, "building admin secret constraint failed")
		}
		// check the current version with the constraint
		flags.HasAccessKey = adminSecretConstraint.Check(v.ServerSemver)

		// create a constraint to check if the current server version has actions
		actionConstraint, err := semver.NewConstraint(">= " + actionVersion)
		if err != nil {
			return errors.Wrap(err, "building action constraint failed")
		}
		// check the current version with the constraint
		flags.HasAction = actionConstraint.Check(v.ServerSemver)
	}
	v.ServerFeatureFlags = flags
	return nil
}
