package projectmetadata

import (
	"bytes"
	"io/ioutil"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/sirupsen/logrus"

	"github.com/hasura/graphql-engine/cli/v2/version"

	"github.com/hasura/graphql-engine/cli/v2"
)

func TestPrintContextRichDiffBetweenProjectDirectories(t *testing.T) {
	type args struct {
		opts PrintContextRichDiffBetweenProjectDirectoriesOpts
	}
	tests := []struct {
		name      string
		args      args
		out       *bytes.Buffer
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"t1",
			args{
				opts: func() PrintContextRichDiffBetweenProjectDirectoriesOpts {
					ec := cli.NewExecutionContext()
					ec.Logger = logrus.New()
					ec.Version = version.New()
					ec.Config = &cli.Config{Version: cli.V3}
					ec.HasMetadataV3 = true
					ec.APIClient = &hasura.Client{}
					ec.Version.ServerFeatureFlags = &version.ServerFeatureFlags{
						HasAccessKey:    false,
						HasAction:       true,
						HasCronTriggers: true,
					}
					p := PrintContextRichDiffBetweenProjectDirectoriesOpts{
						EC:              ec,
						MetadataHandler: NewHandlerFromEC(ec),
						FromDirectory:   "testdata/diff/t1/server/metadata",
						ToDirectory:     "testdata/diff/t1/project/metadata",
						DisableColor:    false,
					}
					return p
				}(),
			},
			new(bytes.Buffer),
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tt.args.opts.Writer = tt.out
			err := PrintContextRichDiffBetweenProjectDirectories(tt.args.opts)
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
			goldenFile := filepath.Join("testdata/diff", tt.name, "want")
			// uncomment to update test snapshot file
			//assert.NoError(t, ioutil.WriteFile(goldenFile, tt.out.Bytes(), os.ModePerm))
			want, err := ioutil.ReadFile(goldenFile)
			assert.NoError(t, err)
			assert.Equal(t, string(want), tt.out.String())
		})
	}
}
