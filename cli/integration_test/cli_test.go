package integrationtest_test

import (
	"io/ioutil"
	"math/rand"
	"os"
	"path/filepath"
	"strconv"
	"testing"
	"time"

	"github.com/hasura/graphql-engine/cli/commands"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli"
	integrationtest "github.com/hasura/graphql-engine/cli/integration_test"
	"github.com/spf13/viper"

	v1 "github.com/hasura/graphql-engine/cli/integration_test/v1"
	v2 "github.com/hasura/graphql-engine/cli/integration_test/v2"
	"github.com/sirupsen/logrus/hooks/test"
)

func init() {
	rand.Seed(time.Now().UTC().UnixNano())
}

func TestCommands(t *testing.T) {
	// Run tests only for config version v1
	t.Run("config=v1", func(t *testing.T) {
		// Initialize ec
		ec := cli.NewExecutionContext()
		ec.Config = &cli.Config{}
		logger, _ := test.NewNullLogger()
		ec.Logger = logger
		ec.Spinner = spinner.New(spinner.CharSets[7], 100*time.Millisecond)
		ec.Spinner.Writer = ioutil.Discard
		ec.Viper = viper.New()

		initDir := filepath.Join(os.TempDir(), "hasura-cli-test-"+strconv.Itoa(rand.Intn(1000)))
		defer os.RemoveAll(initDir)

		// This will prepare the execution context, so no need to run ec.Prepare() on all the other tests
		t.Run("prepare", func(t *testing.T) {
			integrationtest.TestPrepare(t, ec)
		})

		skip(t)
		// This will init the project dir
		t.Run("init command", func(t *testing.T) {
			v1.TestInitCmd(t, ec, initDir)
		})

		skip(t)
		// This will validate the project dir
		t.Run("validate", func(t *testing.T) {
			integrationtest.TestValidate(t, ec)
		})

		skip(t)
		t.Run("console command", func(t *testing.T) {
			v1.TestConsoleCmd(t, ec)
		})

		skip(t)
		t.Run("migrate commands", func(t *testing.T) {
			v1.TestMigrateCmd(t, ec)
		})

		skip(t)
		t.Run("metadata commands", func(t *testing.T) {
			v1.TestMetadataCmd(t, ec)
		})
	})

	// Run tests only for config version v2
	t.Run("config=v2", func(t *testing.T) {
		ec := cli.NewExecutionContext()
		ec.Config = &cli.Config{}
		logger, _ := test.NewNullLogger()
		ec.Logger = logger
		ec.Spinner = spinner.New(spinner.CharSets[7], 100*time.Millisecond)
		ec.Spinner.Writer = ioutil.Discard
		ec.Viper = viper.New()

		initDir := filepath.Join(os.TempDir(), "hasura-cli-test-"+strconv.Itoa(rand.Intn(1000)))
		defer os.RemoveAll(initDir)

		// This will prepare the execution context, so no need to run ec.Prepare() on all the other tests
		t.Run("prepare", func(t *testing.T) {
			integrationtest.TestPrepare(t, ec)
		})

		skip(t)
		// This will init the project dir
		t.Run("init command", func(t *testing.T) {
			v2.TestInitCmd(t, ec, initDir)
		})

		skip(t)
		// This will validate the project dir
		t.Run("validate", func(t *testing.T) {
			integrationtest.TestValidate(t, ec)
		})

		skip(t)
		if cliExtManifestFilePath := os.Getenv("HASURA_GRAPHQL_TEST_CLI_EXT_MANIFEST_FILE_PATH"); cliExtManifestFilePath != "" {
			t.Run("cli-ext-plugin-install", func(t *testing.T) {
				installOpts := &commands.PluginInstallOptions{
					EC:           ec,
					Name:         cli.CLIExtPluginName,
					ManifestFile: cliExtManifestFilePath,
				}
				err := installOpts.Run()
				if err != nil {
					t.Fatalf("unable to install %s plugin, got %v", cli.CLIExtPluginName, err)
				}
			})
		}

		skip(t)
		t.Run("console command", func(t *testing.T) {
			v2.TestConsoleCmd(t, ec)
		})

		skip(t)
		t.Run("migrate commands", func(t *testing.T) {
			v2.TestMigrateCmd(t, ec)
		})

		skip(t)
		t.Run("metadata commands", func(t *testing.T) {
			v2.TestMetadataCmd(t, ec)
		})
	})
}

func skip(t *testing.T) {
	if t.Failed() {
		t.SkipNow()
	}
}
