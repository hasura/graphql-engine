package metadata

import (
	"fmt"
	"io/ioutil"
	"testing"

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
