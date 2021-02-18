package sources

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"gopkg.in/yaml.v3"
)

func Test_resolveTags(t *testing.T) {
	type args struct {
		ctx  map[string]string
		node *yaml.Node
	}
	tests := []struct {
		name    string
		args    args
		want    string
		wantErr bool
	}{
		{
			"can resolve !include tags",
			args{
				ctx: map[string]string{includeTag: "testdata/metadata/"},
				node: func() *yaml.Node {
					v := new(yaml.Node)
					b := []byte(`
actions: !include "actions.yaml"
`)
					assert.NoError(t, yaml.Unmarshal(b, v))
					return v
				}(),
			},
			"actions:\n    actions: []\n    custom_types:\n        enums: []\n        input_objects: []\n        objects: []\n        scalars: []\n",
			false,
		},
		{
			"can resolve !include tags in strings",
			args{
				ctx: map[string]string{includeTag: "testdata/metadata/"},
				node: func() *yaml.Node {
					v := new(yaml.Node)
					b := []byte(`
actions: '!include "actions.yaml"'
`)
					assert.NoError(t, yaml.Unmarshal(b, v))
					return v
				}(),
			},
			"actions:\n    actions: []\n    custom_types:\n        enums: []\n        input_objects: []\n        objects: []\n        scalars: []\n",
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := resolveTags(tt.args.ctx, tt.args.node)
			if (err != nil) != tt.wantErr {
				t.Errorf("resolveTags() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			b, err := yaml.Marshal(got)
			assert.NoError(t, err)
			assert.Equal(t, tt.want, string(b))
		})
	}
}
