package remoteschemas

import (
	"io/ioutil"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/stretchr/testify/assert"
	"gopkg.in/yaml.v2"

	"github.com/sirupsen/logrus"
)

func TestRemoteSchemaConfig_Export(t *testing.T) {
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
			"can create remote schemas metadata files",
			fields{
				MetadataDir: "./metadata",
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
				"metadata/remote_schemas/remote_schemas.yaml": []byte(
					`- name: tt
  definition:
    timeout_seconds: 60
    url: http://host.docker.internal:4000/graphql
  comment: ""
  permissions: "!include tt/permissions/permissions.yaml"
`),
				"metadata/remote_schemas/tt/permissions/permissions.yaml": []byte(
					`- role: manager
  definition:
    schema: "!include schemas/manager.graphql"
- role: user
  definition:
    schema: "!include schemas/user.graphql"
`),
				"metadata/remote_schemas/tt/permissions/schemas/manager.graphql": []byte("schema  { query: Query }\ntype Query { hello: String\n}"),
				"metadata/remote_schemas/tt/permissions/schemas/user.graphql":    []byte("schema  { query: Query }\ntype Query { hello: String\n}"),
			},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tc := &RemoteSchemaConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			got, err := tc.Export(tt.args.metadata)
			if (err != nil) != tt.wantErr {
				t.Fatalf("Export() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			var wantContent = map[string]string{}
			var gotContent = map[string]string{}
			for k, v := range got {
				gotContent[k] = string(v)
			}
			for k, v := range tt.want {
				wantContent[k] = string(v)
			}
			if diff := cmp.Diff(wantContent, gotContent); diff != "" {
				t.Errorf("Export() mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func TestRemoteSchemaConfig_Build(t *testing.T) {
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
			"can build remote schema metadata files from json",
			fields{
				MetadataDir: "testdata/metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: new(yaml.MapSlice),
			},
			`remote_schemas:
- name: tt
  definition:
    timeout_seconds: 60
    url: http://host.docker.internal:4000/graphql
  comment: ""
  permissions:
  - definition:
      schema: |-
        schema  { query: Query }
        type Query { hello: String }
    role: manager
  - definition:
      schema: |-
        schema  { query: Query }
        type Query { hello: String }
    role: user
`,
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tc := &RemoteSchemaConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			if err := tc.Build(tt.args.metadata); (err != nil) != tt.wantErr {
				t.Fatalf("Build() error = %v, wantErr %v", err, tt.wantErr)
			}
			b, err := yaml.Marshal(tt.args.metadata)
			assert.NoError(t, err)
			assert.Equal(t, tt.want, string(b))
		})
	}
}
