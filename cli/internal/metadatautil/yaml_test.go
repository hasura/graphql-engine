package metadatautil

import (
	"io/ioutil"
	"strings"
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
				ctx: map[string]string{baseDirectoryKey: "testdata/metadata/"},
				node: func() *yaml.Node {
					v := new(yaml.Node)
					b := []byte(`
actions: !include "actions.yaml"
`)
					assert.NoError(t, yaml.Unmarshal(b, v))
					return v
				}(),
			},
			`actions:
    actions: []
    custom_types:
        enums: []
        input_objects: []
        objects: []
        scalars: []
`,
			false,
		},
		{
			"can resolve !include tags in strings",
			args{
				ctx: map[string]string{baseDirectoryKey: "testdata/metadata/"},
				node: func() *yaml.Node {
					v := new(yaml.Node)
					b := []byte(`
actions: '!include "actions.yaml"'
`)
					assert.NoError(t, yaml.Unmarshal(b, v))
					return v
				}(),
			},
			`actions:
    actions: []
    custom_types:
        enums: []
        input_objects: []
        objects: []
        scalars: []
`,
			false,
		},
		{
			"can resolve !include tags in strings",
			args{
				ctx: map[string]string{baseDirectoryKey: "testdata/metadata/"},
				node: func() *yaml.Node {
					v := new(yaml.Node)
					b := []byte(`
actions: "!include actions.yaml"
`)
					assert.NoError(t, yaml.Unmarshal(b, v))
					return v
				}(),
			},
			`actions:
    actions: []
    custom_types:
        enums: []
        input_objects: []
        objects: []
        scalars: []
`,
			false,
		},
		{
			"can resolve !include tags with relative paths",
			args{
				ctx: map[string]string{baseDirectoryKey: "testdata/metadata/databases"},
				node: func() *yaml.Node {
					v := new(yaml.Node)
					b, err := ioutil.ReadFile("testdata/metadata/databases/databases.yaml")
					assert.Nil(t, err)
					assert.NoError(t, yaml.Unmarshal(b, v))
					return v
				}(),
			},
			`- name: s1
  kind: postgres
  configuration:
    connection_info:
        database_url:
            from_env: HASURA_GRAPHQL_DATABASE_URL
        isolation_level: read-committed
        pool_settings:
            idle_timeout: 180
            max_connections: 50
            retries: 1
        use_prepared_statements: true
  tables:
    - table:
        name: t1
        schema: public
    - table:
        name: t2
        schema: public
  functions:
    - function:
        name: get_t1
        schema: public
    - function:
        name: get_t2
        schema: public
- name: s2
  kind: postgres
  configuration:
    connection_info:
        database_url:
            from_env: HASURA_GRAPHQL_DATABASE_URL
        isolation_level: read-committed
        pool_settings:
            idle_timeout: 180
            max_connections: 50
            retries: 1
        use_prepared_statements: true
  tables:
    - table:
        name: t1
        schema: public
    - table:
        name: t2
        schema: public
  functions:
    - function:
        name: get_t1
        schema: public
    - function:
        name: get_t2
        schema: public
- name: s 3
  kind: postgres
  configuration:
    connection_info:
        database_url:
            from_env: HASURA_GRAPHQL_DATABASE_URL
        isolation_level: read-committed
        pool_settings:
            idle_timeout: 180
            max_connections: 50
            retries: 1
        use_prepared_statements: true
  tables:
    - table:
        name: t1
        schema: public
    - table:
        name: t2
        schema: public
  functions:
    - function:
        name: get_t1
        schema: public
    - function:
        name: get_t2
        schema: public
- name: s 4
  kind: postgres
  configuration:
    connection_info:
        database_url:
            from_env: HASURA_GRAPHQL_DATABASE_URL
        isolation_level: read-committed
        pool_settings:
            idle_timeout: 180
            max_connections: 50
            retries: 1
        use_prepared_statements: true
  tables:
    - table:
        name: t1
        schema: public
    - table:
        name: t2
        schema: public
  functions:
    - function:
        name: get_t1
        schema: public
    - function:
        name: get_t2
        schema: public
- name: s 5
  kind: postgres
  configuration:
    connection_info:
        database_url:
            from_env: HASURA_GRAPHQL_DATABASE_URL
        isolation_level: read-committed
        pool_settings:
            idle_timeout: 180
            max_connections: 50
            retries: 1
        use_prepared_statements: true
  tables:
    - table:
        name: t1
        schema: public
    - table:
        name: t2
        schema: public
  functions:
    - function:
        name: get_t1
        schema: public
    - function:
        name: get_t2
        schema: public
`,
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := resolveTags(tt.args.ctx, tt.args.node, nil)
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

func TestGetIncludeTagFiles(t *testing.T) {
	type args struct {
		node    *yaml.Node
		baseDir string
	}
	tests := []struct {
		name    string
		args    args
		want    []string
		wantErr bool
	}{
		{
			"can parse !include custom tags and generate child files",
			args{
				func() *yaml.Node {
					var testdoc struct {
						Foo yaml.Node `yaml:"foo"`
					}
					file, err := ioutil.ReadFile("testdata/include_tags_children/root.yaml")
					assert.NoError(t, err)
					assert.NoError(t, yaml.Unmarshal(file, &testdoc))
					return &testdoc.Foo
				}(),
				"testdata/include_tags_children",
			},
			[]string{"testdata/include_tags_children/bar/bar.yaml", "testdata/include_tags_children/bar/foo.yaml"},
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := GetIncludeTagFiles(tt.args.node, tt.args.baseDir)
			if tt.wantErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
			}
			for _, want := range tt.want {
				found := false
				for _, g := range got {
					if strings.Contains(g, want) {
						found = true
						break
					}
				}
				assert.Equalf(t, true, found, "expected to find: %v in %v", want, got)
			}
		})
	}
}
