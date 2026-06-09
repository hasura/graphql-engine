package commands

import (
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2"
)

func TestMigrateApplyOptionsValidate_MutualExclusion(t *testing.T) {
	tests := []struct {
		name                    string
		noTransaction           bool
		perMigrationTransaction bool
		wantErrSubstr           string
	}{
		{
			name:          "--no-transaction only",
			noTransaction: true,
		},
		{
			name:                    "--per-migration-transaction only",
			perMigrationTransaction: true,
		},
		{
			name:                    "both flags set returns error",
			noTransaction:           true,
			perMigrationTransaction: true,
			wantErrSubstr:           "--no-transaction and --per-migration-transaction are mutually exclusive",
		},
		{
			name: "neither flag set",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			opts := &MigrateApplyOptions{
				EC: &cli.ExecutionContext{
					Config: &cli.Config{Version: cli.V2},
				},
				NoTransaction:           tt.noTransaction,
				PerMigrationTransaction: tt.perMigrationTransaction,
			}
			err := opts.Validate()
			if tt.wantErrSubstr == "" {
				if err != nil {
					t.Errorf("Validate() unexpected error: %v", err)
				}
				return
			}
			if err == nil {
				t.Fatalf("Validate() expected error containing %q, got nil", tt.wantErrSubstr)
			}
			if !strings.Contains(err.Error(), tt.wantErrSubstr) {
				t.Errorf("Validate() error = %q, want it to contain %q", err.Error(), tt.wantErrSubstr)
			}
		})
	}
}
