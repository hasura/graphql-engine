package telemetry_test

import (
	"testing"

	"github.com/hasura/graphql-engine/cli/telemetry"
	"github.com/hasura/graphql-engine/cli/version"
)

func TestBeamDev(t *testing.T) {
	tm := telemetry.BuildEvent()
	tm.Version = version.DevVersion
	tm.Command = "TEST"
	tm.CanBeam = true
	tm.Beam()
}

func TestBeamProd(t *testing.T) {
	tm := telemetry.BuildEvent()
	tm.Version = "v1.0.0-a-valid-semver"
	tm.Command = "TEST"
	tm.CanBeam = true
	tm.Beam()
}
