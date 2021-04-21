package inheritedroles

import (
	"io/ioutil"
	"testing"

	goccyaml "github.com/goccy/go-yaml"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"gopkg.in/yaml.v2"
)

func TestInheritedRolesConfig_Build(t *testing.T) {
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
		want    *yaml.MapSlice
		wantErr bool
	}{
		{
			"can build inherited roles",
			fields{
				MetadataDir: "testdata/metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: new(yaml.MapSlice),
			},
			func() *yaml.MapSlice {
				var v = yaml.MapItem{
					Key:   "inherited_roles",
					Value: []yaml.MapSlice{},
				}
				b, err := ioutil.ReadFile("testdata/metadata/inherited_roles.yaml")
				assert.NoError(t, err)
				assert.NoError(t, yaml.Unmarshal(b, &v.Value))
				m := yaml.MapSlice{}
				m = append(m, v)
				return &m
			}(),
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tc := &InheritedRolesConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			err := tc.Build(tt.args.metadata)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, tt.want, tt.args.metadata)
			}
		})
	}
}

func TestInheritedRolesConfig_Export(t *testing.T) {
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
			name: "can export inherited roles",
			fields: fields{
				MetadataDir: "testdata/metadata",
				logger:      logrus.New(),
			},
			args: args{
				metadata: func() yaml.MapSlice {
					metadata := []byte(`{
  "version": 3,
  "inherited_roles": [
    {
      "role_name": "combined_user",
      "role_set": [
        "user",
        "user1"
      ]
    }
  ]
}`)
					yamlb, err := goccyaml.JSONToYAML(metadata)
					assert.NoError(t, err)
					var v yaml.MapSlice
					assert.NoError(t, yaml.Unmarshal(yamlb, &v))
					return v
				}(),
			},
			want: func() map[string][]byte {
				m := map[string][]byte{
					"testdata/metadata/inherited_roles.yaml": []byte(`- role_name: combined_user
  role_set:
  - user
  - user1
`),
				}
				return m
			}(),
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tc := &InheritedRolesConfig{
				MetadataDir: tt.fields.MetadataDir,
				logger:      tt.fields.logger,
			}
			got, err := tc.Export(tt.args.metadata)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				gotS := map[string]string{}
				wantS := map[string]string{}
				for k, v := range got {
					gotS[k] = string(v)
				}
				for k, v := range tt.want {
					wantS[k] = string(v)
				}
				assert.Equal(t, wantS, gotS)
			}
		})
	}
}
