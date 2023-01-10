package cli

import (
	"os"
	"testing"

	"github.com/spf13/afero"
	"github.com/spf13/viper"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestExecutionContext_readAdminSecret(t *testing.T) {
	adminSecretEnv := "HASURA_GRAPHQL_ADMIN_SECRET"
	oldAdminSecret := os.Getenv(adminSecretEnv)
	os.Setenv(adminSecretEnv, "")
	defer func() {
		os.Setenv(adminSecretEnv, oldAdminSecret)
	}()
	fs := afero.NewMemMapFs()
	require.NoError(t, fs.MkdirAll("/project", 0755))
	tests := []struct {
		name             string
		before           func() func()
		wantAdminSecret  string
		wantAdminSecrets []string
		wantErr          bool
		assertErr        require.ErrorAssertionFunc
	}{
		{
			"can set admin secret from config file",
			func() func() {
				configFile := `admin_secret: test`
				require.NoError(t, afero.WriteFile(fs, "/project/config.yml", []byte(configFile), 0644))
				_, err := fs.Stat("/project/config.yml")
				require.NoError(t, err)
				return func() {}
			},
			"test",
			[]string{},
			false,
			require.NoError,
		},
		{
			"can set admin secrets from config file",
			func() func() {
				configFile := `admin_secrets: '["s1","s2","s3"]'`
				require.NoError(t, afero.WriteFile(fs, "/project/config.yml", []byte(configFile), 0644))
				_, err := fs.Stat("/project/config.yml")
				require.NoError(t, err)
				return func() {}
			},
			"",
			[]string{"s1", "s2", "s3"},
			false,
			require.NoError,
		},
		{
			"can set admin secret from env variable",
			func() func() {
				require.NoError(t, afero.WriteFile(fs, "/project/config.yml", []byte(""), 0644))
				oldEnvValue := os.Getenv(adminSecretEnv)
				teardown := func() {
					os.Setenv(adminSecretEnv, oldEnvValue)
				}
				os.Setenv(adminSecretEnv, "test")
				return teardown
			},
			"test",
			[]string{},
			false,
			require.NoError,
		},
		{
			"can set admin secrets from env variable",
			func() func() {
				require.NoError(t, afero.WriteFile(fs, "/project/config.yml", []byte(""), 0644))
				adminSecretEnv := "HASURA_GRAPHQL_ADMIN_SECRETS"
				oldEnvValue := os.Getenv(adminSecretEnv)
				teardown := func() {
					os.Setenv(adminSecretEnv, oldEnvValue)
				}
				os.Setenv(adminSecretEnv, "[\"s1\",\"s2\",\"s3\"]")
				return teardown
			},
			"",
			[]string{"s1", "s2", "s3"},
			false,
			require.NoError,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			teardown := tt.before()
			defer teardown()

			v := viper.New()
			v.SetFs(fs)
			v.AddConfigPath("/project")
			ec := &ExecutionContext{
				Viper: v,
			}
			err := ec.readConfig()
			tt.assertErr(t, err)
			if tt.wantErr {
				return
			}
			assert.Equal(t, tt.wantAdminSecret, ec.Config.AdminSecret)
			assert.Equal(t, tt.wantAdminSecrets, ec.Config.AdminSecrets)

		})
	}
}

func TestServerConfig_GetAdminSecret(t *testing.T) {
	type fields struct {
		adminSecret  string
		adminSecrets []string
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		{
			"admin secrets takes precedence when adminSecret & adminSecrets are set",
			fields{"test", []string{"foo", "bar"}},
			"foo",
		},
		{
			"admin secret is returned when set",
			fields{"test", []string{}},
			"test",
		},
		{
			"empty string is returned when none is set",
			fields{"", []string{}},
			"",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &ServerConfig{
				AdminSecret:  tt.fields.adminSecret,
				AdminSecrets: tt.fields.adminSecrets,
			}
			got := c.GetAdminSecret()
			assert.Equal(t, tt.want, got)
		})
	}
}
