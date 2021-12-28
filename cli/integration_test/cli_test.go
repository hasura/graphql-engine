package integrationtest_test

import (
	"io/ioutil"
	"math/rand"
	"os"
	"path/filepath"
	"strconv"
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli/v2"
	integrationtest "github.com/hasura/graphql-engine/cli/v2/integration_test"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	"github.com/spf13/viper"

	v2 "github.com/hasura/graphql-engine/cli/v2/integration_test/v2"
	v3 "github.com/hasura/graphql-engine/cli/v2/integration_test/v3"
	"github.com/sirupsen/logrus/hooks/test"
)

func init() {
	rand.Seed(time.Now().UTC().UnixNano())
}

func TestCommands(t *testing.T) {
	// Run tests only for config version v2
	t.Run("config=v2", func(t *testing.T) {
		ec := cli.NewExecutionContext()
		ec.Config = &cli.Config{}
		logger, _ := test.NewNullLogger()
		ec.Logger = logger
		ec.Spinner = spinner.New(spinner.CharSets[7], 100*time.Millisecond)
		ec.Spinner.Writer = ioutil.Discard
		ec.Viper = viper.New()
		ec.Stdout = os.Stdout
		ec.Stderr = os.Stderr

		initDir := filepath.Join(os.TempDir(), "hasura-cli-test-"+strconv.Itoa(rand.Intn(1000)))
		defer os.RemoveAll(initDir)

		hasuraPort, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
		defer teardown()

		// This will prepare the execution context, so no need to run ec.Prepare() on all the other tests
		t.Run("prepare", func(t *testing.T) {
			integrationtest.TestPrepare(t, ec)
		})

		skip(t)
		// This will init the project dir
		t.Run("init command", func(t *testing.T) {
			v2.TestInitCmd(t, ec, initDir, hasuraPort)
		})

		skip(t)
		// This will validate the project dir
		t.Run("validate", func(t *testing.T) {
			integrationtest.TestValidate(t, ec)
		})

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

		skip(t)
		t.Run("seed create command", func(t *testing.T) {
			v2.TestSeedsCreateCmd(t, ec)
		})

		skip(t)
		t.Run("seed apply commands", func(t *testing.T) {
			v2.TestSeedsApplyCmd(t, ec)
		})
	})
	t.Run("config=v3", func(t *testing.T) {
		ec := cli.NewExecutionContext()
		ec.Config = &cli.Config{}
		logger, _ := test.NewNullLogger()
		ec.Logger = logger
		ec.Spinner = spinner.New(spinner.CharSets[7], 100*time.Millisecond)
		ec.Spinner.Writer = ioutil.Discard
		ec.Viper = viper.New()
		ec.Stdout = os.Stdout
		ec.Stderr = os.Stderr

		initDir := filepath.Join(os.TempDir(), "hasura-cli-test-"+strconv.Itoa(rand.Intn(1000)))
		defer os.RemoveAll(initDir)

		hasuraPort, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
		defer teardown()

		// This will prepare the execution context, so no need to run ec.Prepare() on all the other tests
		t.Run("prepare", func(t *testing.T) {
			integrationtest.TestPrepare(t, ec)
		})

		skip(t)
		// This will init the project dir
		t.Run("init command", func(t *testing.T) {
			v3.TestInitCmd(t, ec, initDir, hasuraPort)
		})

		skip(t)
		// This will validate the project dir
		t.Run("validate", func(t *testing.T) {
			integrationtest.TestValidate(t, ec)
		})

		skip(t)
		t.Run("console command", func(t *testing.T) {
			v3.TestConsoleCmd(t, ec)
		})

		skip(t)
		t.Run("migrate commands", func(t *testing.T) {
			v3.TestMigrateCmd(t, ec)
		})

		skip(t)
		t.Run("metadata commands", func(t *testing.T) {
			v3.TestMetadataCmd(t, ec)
		})

		skip(t)
		t.Run("seed create command", func(t *testing.T) {
			v3.TestSeedsCreateCmd(t, ec)
		})

		skip(t)
		t.Run("seed apply commands", func(t *testing.T) {
			v3.TestSeedsApplyCmd(t, ec)
		})

		skip(t)
		t.Run("deploy commands", func(t *testing.T) {
			v3.TestDeployCmd(t, ec)
		})
	})
}

func skip(t *testing.T) {
	if t.Failed() {
		t.SkipNow()
	}
}
