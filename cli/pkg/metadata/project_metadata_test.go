package metadata

import (
	"context"
	"fmt"
	"io/ioutil"
	"net/http"
	"path/filepath"
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/pkg/migrate"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
)

func TestProjectMetadataOps_Apply(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	defer teardown()
	type fields struct {
		projectDirectory string
		endpointString   string
	}
	// Named for reuse below. We only care about the inconsistent_objects array
	// as a set, i.e. ordering may change:
	v3Expected :=
		`{"is_consistent":false,"inconsistent_objects":[
			{"definition":{"name":"t4","schema":"pub"},"name":"table pub.t4 in source default","reason":"Inconsistent object: no such table/view exists in source: \"pub.t4\"","type":"table"},
			{"definition":{"name":"t3","schema":"pub"},"name":"table pub.t3 in source default","reason":"Inconsistent object: no such table/view exists in source: \"pub.t3\"","type":"table"},
			{"definition":{"name":"t1","schema":"public"},"name":"table t1 in source default","reason":"Inconsistent object: no such table/view exists in source: \"t1\"","type":"table"},
			{"definition":{"name":"t2","schema":"public"},"name":"table t2 in source default","reason":"Inconsistent object: no such table/view exists in source: \"t2\"","type":"table"}
			]}`
	tests := []struct {
		name      string
		fields    fields
		want      string
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can apply metadata from config v3 project",
			fields{
				projectDirectory: "testdata/projectv3",
				endpointString:   hgeEndpoint,
			},
			v3Expected,
			false,
			require.NoError,
		},
		{
			"can apply metadata from config v3 project in file mode (json)",
			fields{
				projectDirectory: "testdata/projectv3-file-mode-json",
				endpointString:   hgeEndpoint,
			},
			v3Expected,
			false,
			require.NoError,
		},
		{
			"can apply metadata from config v3 project in file mode (yaml)",
			fields{
				projectDirectory: "testdata/projectv3-file-mode-yaml",
				endpointString:   hgeEndpoint,
			},
			v3Expected,
			false,
			require.NoError,
		},
		{
			"can apply metadata from config v2 project",
			fields{
				projectDirectory: "testdata/projectv2",
				endpointString:   hgeEndpoint,
			},
			"",
			true,
			require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &errors.Error{}, err)
				e := err.(*errors.Error)
				require.Equal(t, errors.Op("metadata.ProjectMetadata.Apply"), e.Op)
			}),
		},
		{
			"can apply metadata from config v2 project file mode (json)",
			fields{
				projectDirectory: "testdata/projectv2-file-mode-json",
				endpointString:   hgeEndpoint,
			},
			"",
			true,
			require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &errors.Error{}, err)
				e := err.(*errors.Error)
				require.Equal(t, errors.Op("metadata.ProjectMetadata.Apply"), e.Op)
			}),
		},
		{
			"can apply metadata from config v2 project file mode (yaml)",
			fields{
				projectDirectory: "testdata/projectv2-file-mode-yaml",
				endpointString:   hgeEndpoint,
			},
			"",
			true,
			require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &errors.Error{}, err)
				e := err.(*errors.Error)
				require.Equal(t, errors.Op("metadata.ProjectMetadata.Apply"), e.Op)
			}),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectMetadata(tt.fields.projectDirectory, WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(tt.fields.endpointString))
			require.NoError(t, err)
			got, err := p.Apply()
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
			require.NotNil(t, got)
			gotb, err := ioutil.ReadAll(got)
			require.NoError(t, err)
			require.JSONEq(t, tt.want, string(gotb))
		})
	}
}

func TestProjectMetadataOps_Parse(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	defer teardown()
	type fields struct {
		projectDirectory string
	}
	tests := []struct {
		name       string
		fields     fields
		wantGolden string
		wantErr    bool
		assertErr  require.ErrorAssertionFunc
	}{
		{
			"can generate json metadata from config v3 project",
			fields{
				projectDirectory: "testdata/projectv3",
			},
			"testdata/metadata_parse_test/config-v3.project.want.generated.golden.json",
			false,
			require.NoError,
		},
		{
			"can generate json metadata from config v3 project in filemode (json)",
			fields{
				projectDirectory: "testdata/projectv3-file-mode-json",
			},
			"testdata/metadata_parse_test/config-v3.golden.json",
			false,
			require.NoError,
		},
		{
			"can generate json metadata from config v3 project in filemode (yaml)",
			fields{
				projectDirectory: "testdata/projectv3-file-mode-yaml",
			},
			"testdata/metadata_parse_test/config-v3.golden.json",
			false,
			require.NoError,
		},
		{
			"can generate json metadata from config v2 project",
			fields{
				projectDirectory: "testdata/projectv2",
			},
			"testdata/metadata_parse_test/config-v2.project.want.generated.golden.json",
			false,
			require.NoError,
		},
		{
			"can generate json metadata from config v2 project in filemode (json)",
			fields{
				projectDirectory: "testdata/projectv2-file-mode-json",
			},
			"testdata/metadata_parse_test/config-v2.golden.json",
			false,
			require.NoError,
		},
		{
			"can generate json metadata from config v2 project in filemode (yaml)",
			fields{
				projectDirectory: "testdata/projectv2-file-mode-yaml",
			},
			"testdata/metadata_parse_test/config-v2.golden.json",
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectMetadata(tt.fields.projectDirectory, WithEndpoint(hgeEndpoint), WithAdminSecret(testutil.TestAdminSecret))
			assert.NoError(t, err)
			got, err := p.Parse()
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
			require.NotNil(t, got)
			gotb, err := ioutil.ReadAll(got)
			require.NoError(t, err)
			// uncomment to update golden file
			//assert.NoError(t, ioutil.WriteFile(tt.wantGolden, gotb, os.ModePerm))

			wantb, err := ioutil.ReadFile(tt.wantGolden)
			require.NoError(t, err)
			require.JSONEq(t, string(wantb), string(gotb))
		})
	}
}

func TestProjectMetadataOps_Diff(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	defer teardown()
	type fields struct {
		projectDirectory string
	}
	tests := []struct {
		name       string
		fields     fields
		wantGolden string
		wantErr    bool
		assertErr  require.ErrorAssertionFunc
	}{
		{
			"can generate diff on config v3 project",
			fields{
				projectDirectory: "testdata/projectv3",
			},
			"testdata/metadata_diff_test/config_v3_diff",
			false,
			require.NoError,
		},
		{
			"can generate diff on config v2 project",
			fields{
				projectDirectory: "testdata/projectv2",
			},
			"testdata/metadata_diff_test/config_v2_diff",
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectMetadata(tt.fields.projectDirectory, WithEndpoint(hgeEndpoint), WithAdminSecret(testutil.TestAdminSecret))
			require.NoError(t, err)
			got, err := p.Diff()
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
			gotb, err := ioutil.ReadAll(got)
			require.NoError(t, err)

			// uncomment to update golden file
			//require.NoError(t, ioutil.WriteFile(tt.wantGolden, gotb, os.ModePerm))

			wantb, err := ioutil.ReadFile(tt.wantGolden)
			require.NoError(t, err)
			require.Equal(t, string(wantb), string(gotb))
		})
	}
}

func TestProjectMetadata_Reload(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	defer teardown()
	type fields struct {
		projectDirectory string
		endpointString   string
	}
	tests := []struct {
		name    string
		fields  fields
		want    string
		wantErr require.ErrorAssertionFunc
	}{
		{
			"can reload metadata",
			fields{
				projectDirectory: "testdata/projectv3",
				endpointString:   hgeEndpoint,
			},
			`{
				"is_consistent": true,
				"message": "success"
			}`,
			require.NoError,
		},
		// TODO: automate the following tests
		// following tests are currently ran manually by making use of https://github.com/Shopify/toxiproxy
		//{
		//	"can return expected error type when graphql engine connections fail",
		//	fields{
		//		projectDirectory: "testdata/projectv3",
		//		endpointString:   "http://localhost:12345/something",
		//	},
		//	`{"message": "success"}`,
		//	func(t require.TestingT, err error, i ...interface{}) {
		//		var e *url.Error
		//		require.Truef(t, errors.As(err, &e), "expected err to be an instance of %v but got %v", reflect.TypeOf(&url.Error{}), reflect.TypeOf(err))
		//	},
		//},
		// config.json
		// [
		//  {
		//    "name": "hasura",
		//    "listen": "[::]:18080",
		//    "upstream": "localhost:8080",
		//    "enabled": true
		//  }
		// ]
		//
		// 1. Simulate reset peer
		// $ toxiproxy-cli toxic add -t reset_peer hasura
		// 2. Simulate timeout
		// $ toxiproxy-cli toxic add -t timeout -a timeout=1 hasura
		//{
		//	"return expect error type when graphql engine connections reset",
		//	fields{
		//		projectDirectory: "testdata/projectv3",
		//		endpointString:   "http://localhost:18080",
		//	},
		//	`{"message": "success"}`,
		//	func(t require.TestingT, err error, i ...interface{}) {
		//		var e *url.Error
		//		require.Truef(t, errors.As(err, &e), "expected err to be an instance of %v but got %v", reflect.TypeOf(&url.Error{}), reflect.TypeOf(err))
		//	},
		//},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectMetadata(tt.fields.projectDirectory, WithEndpoint(tt.fields.endpointString), WithAdminSecret(testutil.TestAdminSecret))
			tt.wantErr(t, err)
			if p != nil {
				got, err := p.Reload()

				tt.wantErr(t, err)
				gotb, err := ioutil.ReadAll(got)
				require.NoError(t, err)
				require.JSONEq(t, tt.want, string(gotb))
			}
		})
	}
}

func TestProjectMetadata_GetInconsistentMetadata(t *testing.T) {
	type before func(t *testing.T, p *ProjectMetadata, m *migrate.ProjectMigrate, hgePort, queryEndpoint string)
	configV2Before := func(t *testing.T, metadata *ProjectMetadata, migrations *migrate.ProjectMigrate, hgePort, queryEndpoint string) {
		// - apply all migrations
		// - apply metadata
		// - drop a table via run_sql API
		// - reload metadata
		_, err := migrations.Apply(migrate.ApplyOnAllDatabases())
		require.NoError(t, err)
		_, err = metadata.Apply()
		require.NoError(t, err)

		// remove a table from database
		c := testutil.NewHttpcClient(t, hgePort, nil)
		r, err := c.NewRequest(
			http.MethodPost,
			queryEndpoint,
			hasura.RequestBody{
				Type: "run_sql",
				Args: hasura.PGRunSQLInput{
					SQL:                      "DROP table t1;",
					CheckMetadataConsistency: func() *bool { var v = false; return &v }(),
				},
			},
		)
		require.NoError(t, err)
		resp, err := c.Do(context.Background(), r, nil)
		require.Equal(t, http.StatusOK, resp.StatusCode)
		require.NoError(t, err)

		_, err = metadata.Reload()
		require.NoError(t, err)
	}
	type fields struct {
		projectDirectory string
	}
	tests := []struct {
		name          string
		fields        fields
		before        before
		hasuraImage   string
		queryEndpoint string
		wantErr       bool
		assertErr     require.ErrorAssertionFunc
	}{
		{
			"can list inconsistent metadata config v3",
			fields{
				projectDirectory: "testdata/projectv3",
			},
			func(t *testing.T, metadata *ProjectMetadata, _ *migrate.ProjectMigrate, _ string, _ string) {
				_, err := metadata.Apply()
				require.NoError(t, err)
			},
			testutil.HasuraDockerImage,
			"v2/query",
			false,
			require.NoError,
		},
		{
			"can list inconsistent metadata config v2",
			fields{
				projectDirectory: "testdata/projectv2",
			},
			configV2Before,
			testutil.HasuraDockerImage,
			"v2/query",
			false,
			require.NoError,
		},
		{
			"can list inconsistent metadata config v2 v1.3.3",
			fields{
				projectDirectory: "testdata/projectv2",
			},
			configV2Before,
			"hasura/graphql-engine:v1.3.3",
			"v1/query",
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			port, teardown := testutil.StartHasura(t, tt.hasuraImage)
			hgeEndpoint := fmt.Sprintf("%s:%s", testutil.BaseURL, port)
			defer teardown()

			migrations, err := migrate.NewProjectMigrate(tt.fields.projectDirectory, migrate.WithEndpoint(hgeEndpoint), migrate.WithAdminSecret(testutil.TestAdminSecret))
			require.NoError(t, err)
			metadata, err := NewProjectMetadata(tt.fields.projectDirectory, WithEndpoint(hgeEndpoint), WithAdminSecret(testutil.TestAdminSecret))
			require.NoError(t, err)
			if tt.before != nil {
				tt.before(t, metadata, migrations, port, tt.queryEndpoint)
			}
			got, err := metadata.GetInconsistentMetadata()
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
			gotb, err := ioutil.ReadAll(got)
			require.NoError(t, err)
			goldenFile := filepath.Join("testdata/get_inconsistent_metadata_test", strings.Join(strings.Split(tt.name, " "), "_")+".golden.json")

			// uncomment the following line to update test golden file
			// require.NoError(t, ioutil.WriteFile(goldenFile, gotb, 0655))

			wantb, err := ioutil.ReadFile(goldenFile)
			require.NoError(t, err)
			require.JSONEq(t, string(wantb), string(gotb))
		})
	}
}

func TestProjectMetadata_Export(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	defer teardown()
	type fields struct {
		projectDirectory string
	}

	tests := []struct {
		name      string
		fields    fields
		want      string
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can export metadata from config",
			fields{
				projectDirectory: "testdata/projectv2",
			},
			"testdata/metadata_export_test/config-v2.json",
			false,
			require.NoError,
		},
		{
			"can export json metadata from config in filemode (json)",
			fields{
				projectDirectory: "testdata/projectv2-file-mode-json",
			},
			"testdata/metadata_export_test/config-v2.json",
			false,
			require.NoError,
		},
		{
			"can export yaml metadata from config in filemode (yaml)",
			fields{
				projectDirectory: "testdata/projectv2-file-mode-yaml",
			},
			"testdata/metadata_export_test/config-v2.yaml",
			false,
			require.NoError,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectMetadata(tt.fields.projectDirectory, WithEndpoint(hgeEndpoint), WithAdminSecret(testutil.TestAdminSecret))
			require.NoError(t, err)
			got, err := p.Export()
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
			gotb, err := ioutil.ReadAll(got)
			require.NoError(t, err)

			// uncomment to update golden file
			//require.NoError(t, ioutil.WriteFile(tt.wantGolden, gotb, os.ModePerm))

			wantb, err := ioutil.ReadFile(tt.want)
			require.NoError(t, err)
			require.Equal(t, string(wantb), string(gotb))
		})
	}

}
