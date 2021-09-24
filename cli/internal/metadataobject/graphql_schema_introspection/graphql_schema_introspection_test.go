package graphqlschemaintrospection

import (
	"io/ioutil"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"
)

func TestMetadataObject_Build(t *testing.T) {
	type fields struct {
		MetadataDir string
		logger      *logrus.Logger
	}
	type args struct {
		metadata *yaml.MapSlice
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    string
		wantErr bool
	}{
		{
			"can build from file",
			fields{
				MetadataDir: "testdata/metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: new(yaml.MapSlice),
			},
			`graphql_schema_introspection:
  disabled_for_roles:
  - child
`,
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			m := &MetadataObject{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			err := m.Build(tt.args.metadata)
			if tt.wantErr {
				require.Error(t, err)
			} else {
				b, err := yaml.Marshal(tt.args.metadata)
				assert.NoError(t, err)
				assert.Equal(t, tt.want, string(b))
			}
		})
	}
}

func TestMetadataObject_Export(t *testing.T) {
	type fields struct {
		MetadataDir string
		logger      *logrus.Logger
	}
	type args struct {
		metadata yaml.MapSlice
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    map[string][]byte
		wantErr bool
	}{
		{
			"can export metadata with graphql_schema_introspection",
			fields{
				MetadataDir: "testdata/metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: func() yaml.MapSlice {
					var metadata yaml.MapSlice
					jsonb, err := ioutil.ReadFile("testdata/metadata.json")
					assert.NoError(t, err)
					assert.NoError(t, yaml.Unmarshal(jsonb, &metadata))
					return metadata
				}(),
			},
			map[string][]byte{
				"testdata/metadata/graphql_schema_introspection.yaml": []byte(`disabled_for_roles:
- child
`),
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			obj := &MetadataObject{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			got, err := obj.Export(tt.args.metadata)
			if tt.wantErr {
				require.Error(t, err)
			} else {
				require.NoError(t, err)
				var wantContent = map[string]string{}
				var gotContent = map[string]string{}
				for k, v := range got {
					gotContent[k] = string(v)
				}
				for k, v := range tt.want {
					wantContent[k] = string(v)
				}
				assert.NoError(t, err)
				assert.NoError(t, err)
				if diff := cmp.Diff(wantContent, gotContent); diff != "" {
					t.Errorf("Export() mismatch (-want +got):\n%s", diff)
				}
			}
		})
	}
}
