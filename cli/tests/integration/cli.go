package integrationtest

import (
	"math/rand"
	"testing"
	"time"

	"github.com/hasura/graphql-engine/cli"
)

func init() {
	rand.Seed(time.Now().UTC().UnixNano())
}

func TestPrepare(t *testing.T, ec *cli.ExecutionContext) {
	err := ec.Prepare()
	if err != nil {
		t.Fatalf("prepare failed: %v", err)
	}
	if ec.CMDName == "" {
		t.Fatalf("expected CMDName, got: %v", ec.CMDName)
	}
	if ec.Spinner == nil {
		t.Fatal("got spinner empty")
	}
	if ec.Logger == nil {
		t.Fatal("got empty logger")
	}
	if ec.GlobalConfigDir == "" {
		t.Fatalf("global config dir: expected $HOME/%s, got %s", cli.GlobalConfigDirName, ec.GlobalConfigDir)
	}
	if ec.GlobalConfigFile == "" {
		t.Fatalf("global config file: expected $HOME/%s/%s, got %s", cli.GlobalConfigDirName, cli.GlobalConfigFileName, ec.GlobalConfigFile)
	}
	if ec.Config == nil {
		t.Fatal("got empty Config")
	}
}

func TestValidate(t *testing.T, ec *cli.ExecutionContext) {
	err := ec.Validate()
	if err != nil {
		t.Fatalf("validate failed: %v", err)
	}
}
