package projectmetadata

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/cliext"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/hasura/graphql-engine/cli/v2/version"
	"github.com/mitchellh/go-homedir"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func Test_inconsistentObject_GetName(t *testing.T) {
	type fields struct {
		Definition interface{}
		Reason     interface{}
		Type       interface{}
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		{
			"can decode without a name",
			func() fields {
				var field fields
				b := []byte(`
    {
        "definition": "article",
        "reason": "no such table/view exists in postgres : \"article\"",
        "type": "table"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			"N/A",
		},
		{
			"can decode a with a name",
			func() fields {
				var field fields
				b := []byte(`
    {
        "definition": {
            "using": {
                "foreign_key_constraint_on": {
                    "column": "author_id",
                    "table": "article"
                }
            },
            "name": "articles",
            "comment": null,
            "table": "author"
        },
        "reason": "table \"article\" does not exist",
        "type": "array_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			"articles",
		},
		{
			"can decode a with a name",
			func() fields {
				var field fields
				b := []byte(`
 {
        "definition": {
            "using": {
                "foreign_key_constraint_on": "author_id"
            },
            "name": "author",
            "comment": null,
            "table": "article"
        },
        "reason": "table \"article\" does not exist",
        "type": "object_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			"author",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			obj := InconsistentMetadataObject{
				Definition: tt.fields.Definition,
				Reason:     tt.fields.Reason,
				Type:       tt.fields.Type,
			}
			if got := obj.GetName(); got != tt.want {
				t.Errorf("GetName() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_inconsistentObject_GetDescription(t *testing.T) {
	type fields struct {
		Definition interface{}
		Reason     interface{}
		Type       interface{}
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		{
			"can generate description",
			func() fields {
				var field fields
				b := []byte(`
 {
        "definition": {
            "using": {
                "foreign_key_constraint_on": "author_id"
            },
            "name": "author",
            "comment": null,
            "table": "article"
        },
        "reason": "table \"article\" does not exist",
        "type": "object_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			`{"comment":null,"name":"author","table":"article",...`,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			obj := InconsistentMetadataObject{
				Definition: tt.fields.Definition,
				Reason:     tt.fields.Reason,
				Type:       tt.fields.Type,
			}
			if got := obj.GetDescription(); got != tt.want {
				t.Errorf("GetDescription() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_inconsistentObject_GetType(t *testing.T) {
	type fields struct {
		Definition interface{}
		Reason     interface{}
		Type       interface{}
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		{
			"can get type",
			func() fields {
				var field fields
				b := []byte(`
    {
        "definition": "article",
        "reason": "no such table/view exists in postgres : \"article\"",
        "type": "table"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			"table",
		},
		{
			"can get type",
			func() fields {
				var field fields
				b := []byte(`
    {
        "definition": {
            "using": {
                "foreign_key_constraint_on": {
                    "column": "author_id",
                    "table": "article"
                }
            },
            "name": "articles",
            "comment": null,
            "table": "author"
        },
        "reason": "table \"article\" does not exist",
        "type": "array_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			"array_relation",
		},
		{
			"can get type",
			func() fields {
				var field fields
				b := []byte(`
 {
        "definition": {
            "using": {
                "foreign_key_constraint_on": "author_id"
            },
            "name": "author",
            "comment": null,
            "table": "article"
        },
        "reason": "table \"article\" does not exist",
        "type": "object_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			"object_relation",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			obj := InconsistentMetadataObject{
				Definition: tt.fields.Definition,
				Reason:     tt.fields.Reason,
				Type:       tt.fields.Type,
			}
			if got := obj.GetType(); got != tt.want {
				t.Errorf("GetType() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_inconsistentObject_GetReason(t *testing.T) {
	type fields struct {
		Definition interface{}
		Reason     interface{}
		Type       interface{}
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		{
			"can get reason",
			func() fields {
				var field fields
				b := []byte(`
    {
        "definition": "article",
        "reason": "no such table/view exists in postgres : \"article\"",
        "type": "table"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			`no such table/view exists in postgres : "article"`,
		},
		{
			"can get reason",
			func() fields {
				var field fields
				b := []byte(`
    {
        "definition": {
            "using": {
                "foreign_key_constraint_on": {
                    "column": "author_id",
                    "table": "article"
                }
            },
            "name": "articles",
            "comment": null,
            "table": "author"
        },
        "reason": "table \"article\" does not exist",
        "type": "array_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			`table "article" does not exist`,
		},
		{
			"can get reason",
			func() fields {
				var field fields
				b := []byte(`
 {
        "definition": {
            "using": {
                "foreign_key_constraint_on": "author_id"
            },
            "name": "author",
            "comment": null,
            "table": "article"
        },
        "reason": "table \"article\" does not exist",
        "type": "object_relation"
    }
`)
				if err := json.Unmarshal(b, &field); err != nil {
					t.Fatal(err)
				}
				return field
			}(),
			`table "article" does not exist`,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			obj := InconsistentMetadataObject{
				Definition: tt.fields.Definition,
				Reason:     tt.fields.Reason,
				Type:       tt.fields.Type,
			}
			if got := obj.GetReason(); got != tt.want {
				t.Errorf("GetReason() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestGenMetadataFromMap(t *testing.T) {
	testEC := cli.NewExecutionContext()
	testEC.Version = version.NewCLIVersion(version.DevVersion)
	testEC.Logger = logrus.New()
	home, err := homedir.Dir()
	assert.NoError(t, err)
	testEC.GlobalConfigDir = filepath.Join(home, cli.GlobalConfigDirName)
	testEC.Config = &cli.Config{Version: cli.V3}
	testEC.HasMetadataV3 = true
	assert.NoError(t, cliext.Setup(testEC))

	getObjects := func(metadataDir string) metadataobject.Objects {
		return GetMetadataObjectsWithDir(testEC, metadataDir)
	}
	type args struct {
		metadata map[string]interface{}
	}
	tests := []struct {
		id         string
		name       string
		args       args
		wantGolden string
		wantErr    assert.ErrorAssertionFunc
	}{
		{
			"t1",
			"can generate metadata struct for metadata v3",
			args{
				metadata: func() map[string]interface{} {
					metadataDir := filepath.Join("testdata/test-gen-metadata-from-map/t1/metadata")
					testEC.MetadataDir = metadataDir
					handler := NewHandler(getObjects(metadataDir), nil, nil, logrus.New())
					assert.NoError(t, err)
					metadataMap, err := handler.buildMetadataMap()
					assert.NoError(t, err)
					return metadataMap
				}(),
			},
			"",
			assert.NoError,
		},
		{
			"t2",
			"can generate metadata struct for metadata v2",
			args{
				metadata: func() map[string]interface{} {
					// settings for config v2
					testEC.Config.Version = cli.V2
					testEC.HasMetadataV3 = false

					metadataDir := filepath.Join("testdata/test-gen-metadata-from-map/t2/metadata")
					testEC.MetadataDir = metadataDir
					handler := NewHandler(getObjects(metadataDir), nil, nil, logrus.New())
					assert.NoError(t, err)
					metadataMap, err := handler.buildMetadataMap()
					assert.NoError(t, err)
					return metadataMap
				}(),
			},
			"",
			assert.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := GenMetadataFromMap(tt.args.metadata)
			if !tt.wantErr(t, err, fmt.Sprintf("GenMetadataFromMap(%v)", tt.args.metadata)) {
				return
			}
			gotJSON, err := got.JSON()
			var buf bytes.Buffer
			assert.NoError(t, json.Indent(&buf, gotJSON, "", "  "))
			assert.NoError(t, err)
			goldenFile := filepath.Join("testdata/test-gen-metadata-from-map", tt.id, "want.json")

			// uncomment to update golden file
			assert.NoError(t, ioutil.WriteFile(goldenFile, buf.Bytes(), os.ModePerm))

			wantbs, err := ioutil.ReadFile(goldenFile)
			assert.NoError(t, err)

			assert.Equalf(t, string(wantbs), buf.String(), "GenMetadataFromMap(%v)", tt.args.metadata)
		})
	}
}

func TestBuildMetadataErrorStrict(t *testing.T) {
	testEC := cli.NewExecutionContext()
	testEC.Version = version.NewCLIVersion(version.DevVersion)
	testEC.Logger = logrus.New()
	home, err := homedir.Dir()
	assert.NoError(t, err)
	testEC.GlobalConfigDir = filepath.Join(home, cli.GlobalConfigDirName)
	testEC.Config = &cli.Config{Version: cli.V3}
	testEC.HasMetadataV3 = true
	assert.NoError(t, cliext.Setup(testEC))

	getObjects := func(metadataDir string) metadataobject.Objects {
		return GetMetadataObjectsWithDir(testEC, metadataDir)
	}
	type args struct {
		h *Handler
	}
	tests := []struct {
		name    string
		args    args
		wantErr require.ErrorAssertionFunc
	}{
		{
			"returns correct error type when v3 Metadata files not found",
			args{
				h: func() *Handler {
					metadataDir := filepath.Join("testdata/test-gen-metadata-from-map/") // metadata files don't exist in this folder
					testEC.MetadataDir = metadataDir
					handler := NewHandler(getObjects(metadataDir), nil, nil, logrus.New())
					assert.NoError(t, err)
					return handler
				}(),
			},
			require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.True(t, errors.Is(err, metadataobject.ErrMetadataFileNotFound))
			}),
		},
		{
			"returns correct error type when v2 Metadata files not found",
			args{
				h: func() *Handler {
					testEC.Config.Version = cli.V2
					testEC.HasMetadataV3 = false

					metadataDir := filepath.Join("testdata/test-gen-metadata-from-map/") // metadata files don't exist in this folder
					testEC.MetadataDir = metadataDir
					handler := NewHandler(getObjects(metadataDir), nil, nil, logrus.New())
					assert.NoError(t, err)
					return handler
				}(),
			},
			require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.True(t, errors.Is(err, metadataobject.ErrMetadataFileNotFound))
			}),
		},
	}
	for _, tc := range tests {
		h := tc.args.h
		for _, object := range h.objects {
			_, err := object.Build()
			tc.wantErr(t, err)
		}
	}
}
