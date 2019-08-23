package commands

import (
	"math/rand"
	"os"
	"path/filepath"
	"strconv"
	"testing"
	"time"

	"github.com/briandowns/spinner"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util/fake"
	"github.com/sirupsen/logrus/hooks/test"
)

func init() {
	rand.Seed(time.Now().UTC().UnixNano())
}

func TestInitCmd(t *testing.T) {
	logger, _ := test.NewNullLogger()
	ec := cli.NewExecutionContext()
	ec.Logger = logger
	ec.Spinner = spinner.New(spinner.CharSets[7], 100*time.Millisecond)
	tt := []struct {
		name string
		opts *initOptions
		err  error
	}{
		{"only-init-dir", &initOptions{
			EC:          ec,
			Endpoint:    "",
			AdminSecret: "",
			InitDir:     filepath.Join(os.TempDir(), "hasura-cli-test-"+strconv.Itoa(rand.Intn(1000))),
		}, nil},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			tc.opts.EC.Spinner.Writer = &fake.FakeWriter{}
			err := tc.opts.EC.Prepare()
			if err != nil {
				t.Fatalf("%s: prep failed: %v", tc.name, err)
			}
			err = tc.opts.run()
			if err != tc.err {
				t.Fatalf("%s: expected %v, got %v", tc.name, tc.err, err)
			} else {
				// TODO: (shahidhk) need to verify the contents of the spec generated
				os.RemoveAll(tc.opts.InitDir)
			}
		})
	}
}
