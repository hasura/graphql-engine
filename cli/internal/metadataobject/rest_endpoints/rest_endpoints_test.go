package restendpoints

import (
	"io/ioutil"
	"testing"

	goccyaml "github.com/goccy/go-yaml"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"gopkg.in/yaml.v2"
)

func TestRestEndpointsConfig_Build(t *testing.T) {
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
			"can build rest endpoints",
			fields{
				MetadataDir: "testdata/metadata",
				logger:      logrus.New(),
			},
			args{
				metadata: new(yaml.MapSlice),
			},
			func() *yaml.MapSlice {
				var v = yaml.MapItem{
					Key:   "rest_endpoints",
					Value: []yaml.MapSlice{},
				}
				b, err := ioutil.ReadFile("testdata/metadata/rest_endpoints.yaml")
				assert.NoError(t, err)
				var obj []yaml.MapSlice
				assert.NoError(t, yaml.Unmarshal(b, &obj))
				v.Value = obj
				m := yaml.MapSlice{}
				m = append(m, v)
				return &m
			}(),
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tc := &RestEndpointsConfig{
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

func TestRestEndpointsConfig_Export(t *testing.T) {
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
			name: "can export rest endpoints",
			fields: fields{
				MetadataDir: "testdata/metadata",
				logger:      logrus.New(),
			},
			args: args{
				metadata: func() yaml.MapSlice {
					metadata := []byte(`{
  "version": 3,
  "query_collections": [
    {
      "name": "allowed-queries",
      "definition": {
        "queries": [
          {
            "name": "getAlbum",
            "query": "query getAlbums($id: Int) {\n  albums(where: {id: {_eq: $id}}) {\n    id\n    title\n  }\n}"
          }
        ]
      }
    }
  ],
  "allowlist": [
    {
      "collection": "allowed-queries"
    }
  ],
  "rest_endpoints": [
    {
      "definition": {
        "query": {
          "collection_name": "allowed-queries",
          "query_name": "getAlbum"
        }
      },
      "url": "get_album/:id",
      "methods": [
        "GET"
      ],
      "name": "getAlbum",
      "comment": null
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
					"testdata/metadata/rest_endpoints.yaml": []byte(`- definition:
    query:
      collection_name: allowed-queries
      query_name: getAlbum
  url: get_album/:id
  methods:
  - GET
  name: getAlbum
  comment: null
`),
				}
				return m
			}(),
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tc := &RestEndpointsConfig{
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
