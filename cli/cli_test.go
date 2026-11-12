package cli

import (
	"os"
	"strings"
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
	require.NoError(t, fs.MkdirAll("/project", 0o755))
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
				require.NoError(
					t,
					afero.WriteFile(fs, "/project/config.yml", []byte(configFile), 0o644),
				)
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
				require.NoError(
					t,
					afero.WriteFile(fs, "/project/config.yml", []byte(configFile), 0o644),
				)
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
				require.NoError(t, afero.WriteFile(fs, "/project/config.yml", []byte(""), 0o644))
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
				require.NoError(t, afero.WriteFile(fs, "/project/config.yml", []byte(""), 0o644))
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

func TestExecutionContext_readConfigHeaderFlags(t *testing.T) {
	fs := afero.NewMemMapFs()
	require.NoError(t, fs.MkdirAll("/project", 0o755))
	require.NoError(t, afero.WriteFile(fs, "/project/config.yml", []byte(""), 0o644))

	tests := []struct {
		name       string
		headerFlag []string
		want       map[string]string
		wantErr    bool
	}{
		{
			name:       "no header flags leaves HGEHeaders nil",
			headerFlag: nil,
			want:       nil,
		},
		{
			name:       "valid header flags are parsed into HGEHeaders",
			headerFlag: []string{"Authorization: Bearer jwt", "X-Tenant: acme"},
			want: map[string]string{
				"Authorization": "Bearer jwt",
				"X-Tenant":      "acme",
			},
		},
		{
			name:       "malformed header flag fails config read",
			headerFlag: []string{"no-colon-here"},
			wantErr:    true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := viper.New()
			v.SetFs(fs)
			v.AddConfigPath("/project")
			ec := &ExecutionContext{
				Viper:          v,
				HGEHeaderFlags: tt.headerFlag,
			}
			err := ec.readConfig()
			if tt.wantErr {
				require.Error(t, err)
				return
			}
			require.NoError(t, err)
			assert.Equal(t, tt.want, ec.HGEHeaders)
		})
	}
}

// TestExecutionContext_readConfigPreservesProgrammaticHeaders is the regression test
// for the backward-compat concern that readConfig must not clobber ec.HGEHeaders that an
// embedder pre-populated programmatically. When no --header flags are given the
// pre-populated map must survive; when both are present the flag values merge in and take
// precedence on a name conflict.
func TestExecutionContext_readConfigPreservesProgrammaticHeaders(t *testing.T) {
	fs := afero.NewMemMapFs()
	require.NoError(t, fs.MkdirAll("/project", 0o755))
	require.NoError(t, afero.WriteFile(fs, "/project/config.yml", []byte(""), 0o644))

	tests := []struct {
		name         string
		prepopulated map[string]string
		headerFlag   []string
		want         map[string]string
	}{
		{
			name:         "programmatic headers preserved when no flags given",
			prepopulated: map[string]string{"X-Programmatic": "set-in-code"},
			headerFlag:   nil,
			want:         map[string]string{"X-Programmatic": "set-in-code"},
		},
		{
			name:         "flags merge into programmatic headers",
			prepopulated: map[string]string{"X-Programmatic": "set-in-code"},
			headerFlag:   []string{"Authorization: Bearer jwt"},
			want: map[string]string{
				"X-Programmatic": "set-in-code",
				"Authorization":  "Bearer jwt",
			},
		},
		{
			name:         "flag value takes precedence over programmatic on name conflict",
			prepopulated: map[string]string{"Authorization": "programmatic"},
			headerFlag:   []string{"Authorization: Bearer jwt"},
			want:         map[string]string{"Authorization": "Bearer jwt"},
		},
		{
			name:         "flag wins over case-differing programmatic header (lower prepopulated, canonical flag)",
			prepopulated: map[string]string{"authorization": "programmatic"},
			headerFlag:   []string{"Authorization: Bearer jwt"},
			// Only the flag's key/value remains; the lower-cased programmatic key is dropped.
			want: map[string]string{"Authorization": "Bearer jwt"},
		},
		{
			name:         "flag wins over case-differing programmatic header (canonical prepopulated, lower flag)",
			prepopulated: map[string]string{"Authorization": "programmatic"},
			headerFlag:   []string{"authorization: Bearer jwt"},
			want:         map[string]string{"authorization": "Bearer jwt"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			v := viper.New()
			v.SetFs(fs)
			v.AddConfigPath("/project")
			ec := &ExecutionContext{
				Viper:          v,
				HGEHeaders:     tt.prepopulated,
				HGEHeaderFlags: tt.headerFlag,
			}
			require.NoError(t, ec.readConfig())
			assert.Equal(t, tt.want, ec.HGEHeaders)

			// There must be at most one key for any header name, compared
			// case-insensitively, so the value sent is unambiguous regardless of map
			// iteration order.
			seen := map[string]bool{}
			for k := range ec.HGEHeaders {
				lower := strings.ToLower(k)
				assert.Falsef(t, seen[lower],
					"duplicate case-insensitive key for %q in merged HGEHeaders", k)
				seen[lower] = true
			}
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
