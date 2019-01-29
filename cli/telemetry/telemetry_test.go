package telemetry_test

import (
	"testing"

	"github.com/hasura/graphql-engine/cli/telemetry"
)

func TestBeamDev(t *testing.T) {
	tm := telemetry.BuildEvent()
	tm.Version = "dev"
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
