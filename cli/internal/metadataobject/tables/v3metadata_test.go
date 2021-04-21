package tables

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/sirupsen/logrus"

	"gopkg.in/yaml.v2"
)

func TestV3MetadataTableConfig_Export(t *testing.T) {
	type fields struct {
		TableConfig *TableConfig
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
			name: "can build tables from v3 metadata",
			fields: fields{
				TableConfig: &TableConfig{
					MetadataDir: "testdata/metadata",
					logger:      logrus.New(),
				},
			},
			args: args{
				metadata: func() yaml.MapSlice {
					metadata := `
sources:
-  name: default
   tables:
     - table:
         name: "test"
     - table:
         name: "test"
`
					var v yaml.MapSlice
					assert.NoError(t, yaml.Unmarshal([]byte(metadata), &v))
					return v
				}(),
			},
			want: map[string]string{
				"testdata/metadata/tables.yaml": "- table:\n    name: test\n- table:\n    name: test\n",
			},
		},
		{
			name: "can build metadata when tables is empty",
			fields: fields{
				TableConfig: &TableConfig{
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
				"testdata/metadata/tables.yaml": "[]\n",
			},
		},
		{
			name: "can build metadata when sources is not present",
			fields: fields{
				TableConfig: &TableConfig{
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
				"testdata/metadata/tables.yaml": "[]\n",
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t1 *testing.T) {
			tc := &V3MetadataTableConfig{
				TableConfig: tt.fields.TableConfig,
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
