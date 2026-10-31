package commands

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestTerminateSQL(t *testing.T) {
	tests := []struct {
		name string
		sql  string
		want string
	}{
		{"empty", "", ""},
		{"already terminated", "DROP TABLE a;", "DROP TABLE a;"},
		{"already terminated with trailing newline", "DROP TABLE a;\n", "DROP TABLE a;\n"},
		{"missing semicolon", `ALTER TABLE "public"."todos" DROP COLUMN "completed_at" CASCADE`, `ALTER TABLE "public"."todos" DROP COLUMN "completed_at" CASCADE;`},
		{"missing semicolon with trailing whitespace", "DROP TABLE a\r\n  ", "DROP TABLE a;"},
		{"trailing line comment", "DROP TABLE a\n-- done", "DROP TABLE a\n-- done\n;"},
		{"trailing inline comment", "DROP TABLE a -- done", "DROP TABLE a -- done\n;"},
		{"double dash inside string literal", "SELECT '--'", "SELECT '--'\n;"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			assert.Equal(t, tc.want, terminateSQL(tc.sql))
		})
	}
}
