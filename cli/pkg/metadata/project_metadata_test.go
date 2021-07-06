package metadata

import (
	"context"
	"fmt"
	"io/ioutil"
	"net/http"
	"path/filepath"
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/hasura/graphql-engine/cli/v2/pkg/migrate"

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
	tests := []struct {
		name    string
		fields  fields
		want    string
		wantErr bool
	}{
		{
			"can apply metadata from config v3 project",
			fields{
				projectDirectory: "testdata/projectv3",
				endpointString:   hgeEndpoint,
			},
			`{"is_consistent":false,"inconsistent_objects":[{"definition":{"name":"t1","schema":"public"},"name":"table t1 in source default","reason":"Inconsistent object: no such table/view exists in source: \"t1\"","type":"table"},{"definition":{"name":"t2","schema":"public"},"name":"table t2 in source default","reason":"Inconsistent object: no such table/view exists in source: \"t2\"","type":"table"},{"definition":{"name":"t4","schema":"pub"},"name":"table pub.t4 in source default","reason":"Inconsistent object: no such table/view exists in source: \"pub.t4\"","type":"table"},{"definition":{"name":"t3","schema":"pub"},"name":"table pub.t3 in source default","reason":"Inconsistent object: no such table/view exists in source: \"pub.t3\"","type":"table"}]}`,
			false,
		},
		{
			"can apply metadata from config v2 project",
			fields{
				projectDirectory: "testdata/projectv2",
				endpointString:   hgeEndpoint,
			},
			"",
			true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectMetadata(tt.fields.projectDirectory, WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(tt.fields.endpointString))
			require.NoError(t, err)
			got, err := p.Apply()
			if tt.wantErr {
				require.Error(t, err)
			} else {
				require.NoError(t, err)
				require.NotNil(t, got)

				gotb, err := ioutil.ReadAll(got)
				require.NoError(t, err)
				require.JSONEq(t, tt.want, string(gotb))
			}
		})
	}
}

func TestProjectMetadataOps_Parse(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	defer teardown()
	type fields struct {
		projectDirectory string
		adminSecret      string
		endpointString   string
	}
	tests := []struct {
		name       string
		fields     fields
		wantGolden string
		wantErr    bool
	}{
		{
			"can generate json metadata from config v3 project",
			fields{
				projectDirectory: "testdata/projectv3",
			},
			"testdata/metadata_parse_test/config-v3.golden.json",
			false,
		},
		{
			"can generate json metadata from config v2 project",
			fields{
				projectDirectory: "testdata/projectv2",
			},
			"testdata/metadata_parse_test/config-v2.golden.json",
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectMetadata(tt.fields.projectDirectory, WithEndpoint(hgeEndpoint), WithAdminSecret(testutil.TestAdminSecret))
			require.NoError(t, err)
			got, err := p.Parse()
			if tt.wantErr {
				require.Error(t, err)
			} else {
				require.NoError(t, err)
				require.NotNil(t, got)
				gotb, err := ioutil.ReadAll(got)
				require.NoError(t, err)
				wantb, err := ioutil.ReadFile(tt.wantGolden)
				require.NoError(t, err)

				require.JSONEq(t, string(wantb), string(gotb))
			}
		})
	}
}

func TestProjectMetadataOps_Diff(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	defer teardown()
	type fields struct {
		projectDirectory string
		adminSecret      string
		endpointString   string
	}
	tests := []struct {
		name       string
		fields     fields
		wantGolden string
		wantErr    bool
	}{
		{
			"can generate diff on config v3 project",
			fields{
				projectDirectory: "testdata/projectv3",
			},
			"testdata/metadata_diff_test/config_v3_diff",
			false,
		},
		{
			"can generate diff on config v2 project",
			fields{
				projectDirectory: "testdata/projectv2",
			},
			"testdata/metadata_diff_test/config_v2_diff",
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectMetadata(tt.fields.projectDirectory, WithEndpoint(hgeEndpoint), WithAdminSecret(testutil.TestAdminSecret))
			require.NoError(t, err)
			got, err := p.Diff()
			if tt.wantErr {
				require.Error(t, err)
			}
			gotb, err := ioutil.ReadAll(got)
			require.NoError(t, err)
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
		adminSecret      string
		endpointString   string
	}
	tests := []struct {
		name    string
		fields  fields
		want    string
		wantErr bool
	}{
		{
			"can reload metadata",
			fields{
				projectDirectory: "testdata/projectv3",
				endpointString:   hgeEndpoint,
			},
			`{"message": "success"}`,
			false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectMetadata(tt.fields.projectDirectory, WithEndpoint(hgeEndpoint), WithAdminSecret(testutil.TestAdminSecret))
			require.NoError(t, err)
			got, err := p.Reload()
			if tt.wantErr {
				require.Error(t, err)
			}
			require.NoError(t, err)
			gotb, err := ioutil.ReadAll(got)
			require.NoError(t, err)
			require.JSONEq(t, tt.want, string(gotb))
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
		err := migrations.Apply(migrate.ApplyOnAllDatabases())
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
			if tt.wantErr {
				require.Error(t, err)
			}
			require.NoError(t, err)
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
