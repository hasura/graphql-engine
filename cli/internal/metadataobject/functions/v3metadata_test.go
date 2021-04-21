package functions

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/sirupsen/logrus"

	"gopkg.in/yaml.v2"
)

func TestV3MetadataTableConfig_Export(t *testing.T) {
	type fields struct {
		FunctionConfig *FunctionConfig
	}
	type args struct {
		metadata yaml.MapSlice
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    map[string]string
		wantErr bool
	}{
		{
			name: "can build functions from v3 metadata",
			fields: fields{
				FunctionConfig: &FunctionConfig{
					MetadataDir: "testdata/metadata",
					logger:      logrus.New(),
				},
			},
			args: args{
				metadata: func() yaml.MapSlice {
					metadata := `
sources:
-  name: default
   functions:
     - function:
         name: "test"
     - function:
         name: "test"
`
					var v yaml.MapSlice
					assert.NoError(t, yaml.Unmarshal([]byte(metadata), &v))
					return v
				}(),
			},
			want: map[string]string{
				"testdata/metadata/functions.yaml": "- function:\n    name: test\n- function:\n    name: test\n",
			},
		},
		{
			name: "can build metadata when functions is empty",
			fields: fields{
				FunctionConfig: &FunctionConfig{
					MetadataDir: "testdata/metadata",
					logger:      logrus.New(),
				},
			},
			args: args{
				metadata: func() yaml.MapSlice {
					metadata := `
sources:
-  name: default
`
					var v yaml.MapSlice
					assert.NoError(t, yaml.Unmarshal([]byte(metadata), &v))
					return v
				}(),
			},
			want: map[string]string{
				"testdata/metadata/functions.yaml": "[]\n",
			},
		},
		{
			name: "can build metadata when sources is not present",
			fields: fields{
				FunctionConfig: &FunctionConfig{
					MetadataDir: "testdata/metadata",
					logger:      logrus.New(),
				},
			},
			args: args{
				metadata: func() yaml.MapSlice {
					metadata := ``
					var v yaml.MapSlice
					assert.NoError(t, yaml.Unmarshal([]byte(metadata), &v))
					return v
				}(),
			},
			want: map[string]string{
				"testdata/metadata/functions.yaml": "[]\n",
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t1 *testing.T) {
			tc := &V3MetadataFunctionConfig{
				FunctionConfig: tt.fields.FunctionConfig,
			}
			got, err := tc.Export(tt.args.metadata)
			if (err != nil) != tt.wantErr {
				t.Fatalf("Export() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			var gotS = map[string]string{}
			for k, v := range got {
				gotS[k] = string(v)
			}
			assert.Equal(t, tt.want, gotS)
		})
	}
}
