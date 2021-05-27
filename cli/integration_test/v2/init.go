package v2

import (
	"fmt"
	"os"
	"testing"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/commands"
)

func TestInitCmd(t *testing.T, ec *cli.ExecutionContext, initDir, hasuraPort string) {
	tt := []struct {
		name string
		opts *commands.InitOptions
		err  error
	}{
		{"only-init-dir", &commands.InitOptions{
			EC:          ec,
			Version:     cli.V2,
			Endpoint:    fmt.Sprintf("http://localhost:%s", hasuraPort),
			AdminSecret: os.Getenv("HASURA_GRAPHQL_TEST_ADMIN_SECRET"),
			InitDir:     initDir,
		}, nil},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			err := tc.opts.Run()
			if err != tc.err {
				t.Fatalf("%s: expected %v, got %v", tc.name, tc.err, err)
			}
			// TODO: (shahidhk) need to verify the contents of the spec generated
		})
	}
}
