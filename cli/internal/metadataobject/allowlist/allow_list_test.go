package allowlist

import (
	"bytes"
	"io/ioutil"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/sirupsen/logrus"
)

func TestAllowListConfig_WriteDiff(t *testing.T) {
	type fields struct {
		MetadataDir string
		logger      *logrus.Logger
	}
	type args struct {
		to metadataobject.Object
	}
	tests := []struct {
		name   string
		fields fields
		args   args
		want   metadataobject.ErrParsingMetadataObject
	}{
		{
			"t1",
			fields{"testdata/write_diff/t1/from", logrus.New()},
			args{
				to: func() metadataobject.Object {
					return &AllowListConfig{
						MetadataDir: "testdata/write_diff/t1/to",
						logger:      logrus.New(),
					}
				}(),
			},
			nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			a := &AllowListConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			w := &bytes.Buffer{}
			got := a.WriteDiff(metadataobject.WriteDiffOpts{
				To:           tt.args.to,
				W:            w,
				DisableColor: true,
			})
			wantGoldenFilePath := filepath.Join("testdata/write_diff", tt.name, "want.golden")

			// uncomment to update test golden file
			//assert.NoError(t, ioutil.WriteFile(wantGoldenFilePath, w.Bytes(), os.ModePerm))

			want, err := ioutil.ReadFile(wantGoldenFilePath)
			assert.NoError(t, err)
			assert.Equal(t, string(want), w.String())
			assert.Equal(t, tt.want, got)
		})
	}
}
