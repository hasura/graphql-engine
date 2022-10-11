package metadataobject

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"

	"gopkg.in/yaml.v3"

	"github.com/gonvenience/ytbx"
)

func Test_createEmptyYamlFileAccordingToContent(t *testing.T) {
	type args struct {
		file ytbx.InputFile
	}
	tests := []struct {
		name string
		args args
		want yaml.Kind
	}{
		{
			"can generate sequence node",
			args{
				file: func() ytbx.InputFile {
					m := []map[string]string{{"test": "test"}, {"test2": "test2"}}
					b, err := yaml.Marshal(m)
					assert.NoError(t, err)
					documents, err := ytbx.LoadYAMLDocuments(b)
					assert.NoError(t, err)
					return ytbx.InputFile{
						Documents: documents,
					}
				}(),
			},
			yaml.SequenceNode,
		},
		{

			"can generate mapping node",
			args{
				file: func() ytbx.InputFile {
					m := map[string]string{"test": "test", "test2": "test2"}
					b, err := yaml.Marshal(m)
					assert.NoError(t, err)
					documents, err := ytbx.LoadYAMLDocuments(b)
					assert.NoError(t, err)
					return ytbx.InputFile{
						Documents: documents,
					}
				}(),
			},
			yaml.MappingNode,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := createEmptyYamlFileAccordingToContent(tt.args.file)
			assert.Equal(t, tt.want, got.Documents[0].Content[0].Kind)
		})
	}
}

func Test_cleanExcludedPatterns(t *testing.T) {
	type args struct {
		files    []string
		patterns []string
	}
	tests := []struct {
		name    string
		args    args
		want    []string
		wantErr assert.ErrorAssertionFunc
	}{
		{
			"t1",
			args{
				[]string{"/home/someuser/hasura/metadata/databases/somesource/tables/tables.yaml", "/home/someuser/hasura/metadata/databases/somesource/functions/functions.yaml"},
				[]string{"tables/tables.yaml", "functions/functions.yaml"},
			},
			[]string(nil),
			assert.NoError,
		},
		{
			"t2",
			args{
				[]string{"/home/someuser/hasura/metadata/remote_schemas/permissions/permissions.yaml", "/home/someuser/hasura/metadata/databases/databases.yaml"},
				[]string{"permissions/permissions.yaml"},
			},
			[]string{"/home/someuser/hasura/metadata/databases/databases.yaml"},
			assert.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := cleanExcludedPatterns(tt.args.files, tt.args.patterns)
			if !tt.wantErr(t, err, fmt.Sprintf("cleanExcludedPatterns(%v, %v)", tt.args.files, tt.args.patterns)) {
				return
			}
			assert.Equalf(t, tt.want, got, "cleanExcludedPatterns(%v, %v)", tt.args.files, tt.args.patterns)
		})
	}
}
