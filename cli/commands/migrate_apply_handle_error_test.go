package commands

import (
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2"
)

func TestMigrateApplyOptionsHandleApplyError_MissingMigrationsDirectory(t *testing.T) {
	tests := []struct {
		name           string
		allDatabases   bool
		wantErr        bool
		wantMsgSubstr  string
		wantErrSubstrs []string
	}{
		{
			name:          "--all-databases skips the database",
			allDatabases:  true,
			wantMsgSubstr: "skipping applying migrations on database 'missing'",
		},
		{
			name:           "single database fails",
			allDatabases:   false,
			wantErr:        true,
			wantErrSubstrs: []string{"skipping applying migrations on database 'missing'", "expected to find a migrations directory"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			opts := &MigrateApplyOptions{
				EC:     &cli.ExecutionContext{AllDatabases: tt.allDatabases},
				Source: cli.Source{Name: "missing"},
			}
			msg, err := opts.handleApplyError(&errDatabaseMigrationDirectoryNotFound{
				"expected to find a migrations directory for database 'missing' in migrations, but encountered error: not found",
			})

			if tt.wantErr {
				if err == nil {
					t.Fatalf("expected an error, got message %q", msg)
				}
				for _, substr := range tt.wantErrSubstrs {
					if !strings.Contains(err.Error(), substr) {
						t.Errorf("error %q does not contain %q", err.Error(), substr)
					}
				}
				return
			}

			if err != nil {
				t.Fatalf("expected no error, got %v", err)
			}
			if !strings.Contains(msg, tt.wantMsgSubstr) {
				t.Errorf("message %q does not contain %q", msg, tt.wantMsgSubstr)
			}
		})
	}
}
