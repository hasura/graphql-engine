package telemetry_test

import (
	"testing"

	"github.com/hasura/graphql-engine/cli/telemetry"
	"github.com/hasura/graphql-engine/cli/version"
)

func TestBeam(t *testing.T) {
	tm := telemetry.BuildEvent()
	tm.Version = version.BuildVersion
	tm.Command = "TEST"
	tm.CanBeam = true
	tm.Beam()
}
