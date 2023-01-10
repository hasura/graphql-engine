package deploy

import (
	"fmt"
	"github.com/hasura/graphql-engine/cli/v2/internal/testutil"
	"github.com/stretchr/testify/require"
	"testing"
)

func TestProjectDeploy_ConfigV3(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	defer teardown()
	type fields struct {
		projectDirectory string
		endpointString   string
	}
	tests := []struct {
		name      string
		fields    fields
		want      string
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can deploy project from V3 config",
			fields{
				projectDirectory: "testdata/projectV3",
				endpointString:   hgeEndpoint,
			},
			``,
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectDeploy(tt.fields.projectDirectory, WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(tt.fields.endpointString))
			require.NoError(t, err)
			err = p.Deploy()
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
		})
	}
}

func TestProjectDeploy_ConfigV3_WithSeeds(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	defer teardown()
	type fields struct {
		projectDirectory string
		endpointString   string
	}
	tests := []struct {
		name      string
		fields    fields
		want      string
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can deploy project from V3 config",
			fields{
				projectDirectory: "testdata/projectV3",
				endpointString:   hgeEndpoint,
			},
			``,
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectDeploy(tt.fields.projectDirectory, WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(tt.fields.endpointString))
			require.NoError(t, err)
			err = p.Deploy(WithSeeds())
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
		})
	}
}

func TestProjectDeploy_ConfigV2(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	defer teardown()
	type fields struct {
		projectDirectory string
		endpointString   string
	}
	tests := []struct {
		name      string
		fields    fields
		want      string
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can deploy project from V2 config",
			fields{
				projectDirectory: "testdata/projectV2",
				endpointString:   hgeEndpoint,
			},
			``,
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectDeploy(tt.fields.projectDirectory, WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(tt.fields.endpointString))
			require.NoError(t, err)
			err = p.Deploy()
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
		})
	}
}

func TestProjectDeploy_ConfigV2_WithSeeds(t *testing.T) {
	port, teardown := testutil.StartHasura(t, testutil.HasuraDockerImage)
	hgeEndpoint := fmt.Sprintf("http://localhost:%s", port)
	defer teardown()
	type fields struct {
		projectDirectory string
		endpointString   string
	}
	tests := []struct {
		name      string
		fields    fields
		want      string
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"can deploy project from V2 config",
			fields{
				projectDirectory: "testdata/projectV2",
				endpointString:   hgeEndpoint,
			},
			``,
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewProjectDeploy(tt.fields.projectDirectory, WithAdminSecret(testutil.TestAdminSecret), WithEndpoint(tt.fields.endpointString))
			require.NoError(t, err)
			err = p.Deploy(WithSeeds())
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
		})
	}
}
