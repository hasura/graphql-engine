package commands

import (
	"math/rand"
	"os"
	"path/filepath"
	"strconv"
	"testing"
	"time"

	"github.com/hasura/graphql-engine/cli"
)

func init() {
	rand.Seed(time.Now().UTC().UnixNano())
}

func TestInitCmd(t *testing.T) {
	tt := []struct {
		name string
		opts *initOptions
		err  error
	}{
		{"only-init-dir", &initOptions{
			EC:        &cli.ExecutionContext{},
			Endpoint:  "",
			AccessKey: "",
			InitDir:   filepath.Join(os.TempDir(), "hasura-cli-test-"+strconv.Itoa(rand.Intn(1000))),
		}, nil},
		{"with-endpoint-flag", &initOptions{
			EC:        &cli.ExecutionContext{},
			Endpoint:  "https://localhost:8080",
			AccessKey: "",
			InitDir:   filepath.Join(os.TempDir(), "hasura-cli-test-"+strconv.Itoa(rand.Intn(1000))),
		}, nil},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
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
