package cli

import (
	"testing"

	"github.com/sirupsen/logrus"
	"github.com/sirupsen/logrus/hooks/test"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func telemetryNoticeShown(hook *test.Hook) bool {
	for _, entry := range hook.AllEntries() {
		if entry.Message == TelemetryNotice {
			return true
		}
	}
	return false
}

func TestSetupGlobalConfigTelemetryNotice(t *testing.T) {
	tests := []struct {
		name         string
		telemetryEnv string
		wantNotice   bool
	}{
		{"telemetry enabled by default", "", true},
		{"telemetry disabled through the environment", "false", false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if tc.telemetryEnv != "" {
				t.Setenv("HASURA_GRAPHQL_ENABLE_TELEMETRY", tc.telemetryEnv)
			}
			logger, hook := test.NewNullLogger()
			logger.SetLevel(logrus.DebugLevel)
			ec := NewExecutionContext()
			ec.Logger = logger
			ec.GlobalConfigDir = t.TempDir()

			// first run: the global config file does not exist yet
			require.NoError(t, ec.setupGlobalConfig())
			assert.Equal(t, tc.wantNotice, telemetryNoticeShown(hook))
			assert.Equal(t, tc.wantNotice, ec.GlobalConfig.EnableTelemetry)

			// later runs never show it
			hook.Reset()
			ec.GlobalConfig = nil
			require.NoError(t, ec.setupGlobalConfig())
			assert.False(t, telemetryNoticeShown(hook))
		})
	}
}
