package cli_test

import (
	"math/rand"
	"os"
	"path/filepath"
	"strconv"
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/commands"
	"github.com/hasura/graphql-engine/cli/util/fake"
	"github.com/sirupsen/logrus/hooks/test"
	"github.com/spf13/viper"
)

func init() {
	rand.Seed(time.Now().UTC().UnixNano())
}

func TestPrepare(t *testing.T) {
	logger, _ := test.NewNullLogger()
	ec := cli.NewExecutionContext()
	ec.Logger = logger
	ec.Spinner = spinner.New(spinner.CharSets[7], 100*time.Millisecond)
	ec.Spinner.Writer = &fake.FakeWriter{}
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
	if ec.ServerConfig == nil {
		t.Fatal("got empty Config")
	}
}

func TestValidate(t *testing.T) {
	logger, _ := test.NewNullLogger()
	ec := cli.NewExecutionContext()
	ec.Logger = logger
	ec.Spinner = spinner.New(spinner.CharSets[7], 100*time.Millisecond)
	ec.Spinner.Writer = &fake.FakeWriter{}
	ec.ExecutionDirectory = filepath.Join(os.TempDir(), "hasura-gql-tests-"+strconv.Itoa(rand.Intn(1000)))
	ec.Viper = viper.New()

	// validate a directory created by init
	initCmd := commands.NewInitCmd(ec)
	initCmd.Flags().Set("directory", ec.ExecutionDirectory)
	err := initCmd.Execute()
	if err != nil {
		t.Fatalf("execution failed: %v", err)
	}
	err = ec.Prepare()
	if err != nil {
		t.Fatalf("prepare failed: %v", err)
	}
	err = ec.Validate()
	if err != nil {
		t.Fatalf("validate failed: %v", err)
	}

	// remove config.yaml and validate, should result in an error
	err = os.Remove(filepath.Join(ec.ExecutionDirectory, "config.yaml"))
	if err != nil {
		t.Fatalf("remove failed: %v", err)
	}
	err = ec.Validate()
	if err == nil {
		t.Fatal("validate succeeded with no config.yaml")
	}

	os.RemoveAll(ec.ExecutionDirectory)
}
