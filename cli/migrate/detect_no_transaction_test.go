package migrate

import (
	"io"
	"strings"
	"testing"
)

func TestDetectNoTransactionComment(t *testing.T) {
	tests := []struct {
		name     string
		content  string
		wantNoTx bool
	}{
		// --- positive cases ---
		{
			name:     "marker on first line",
			content:  "-- hasura:no-transaction\nCREATE INDEX CONCURRENTLY idx ON t(c);",
			wantNoTx: true,
		},
		{
			name:     "marker on first line with leading whitespace",
			content:  "   -- hasura:no-transaction\nCREATE TABLE t (id SERIAL);",
			wantNoTx: true,
		},
		{
			name:     "marker on first line with trailing whitespace",
			content:  "-- hasura:no-transaction   \nCREATE TABLE t (id SERIAL);",
			wantNoTx: true,
		},
		{
			name:     "windows CRLF line ending",
			content:  "-- hasura:no-transaction\r\nCREATE TABLE t (id SERIAL);",
			wantNoTx: true,
		},

		// --- negative cases: not present ---
		{
			name:     "no marker",
			content:  "CREATE TABLE users (id SERIAL PRIMARY KEY);",
			wantNoTx: false,
		},
		{
			name:     "empty file",
			content:  "",
			wantNoTx: false,
		},
		{
			name:     "marker not on first line (blank line before it)",
			content:  "\n-- hasura:no-transaction\nCREATE TABLE t (id SERIAL);",
			wantNoTx: false,
		},
		{
			name:     "marker not on first line (other comment before it)",
			content:  "-- some header\n-- hasura:no-transaction\nSELECT 1;",
			wantNoTx: false,
		},
		{
			name:     "partial marker",
			content:  "-- hasura:no-transact\nSELECT 1;",
			wantNoTx: false,
		},
		{
			name:     "no space between dashes and marker text",
			content:  "--hasura:no-transaction\nSELECT 1;",
			wantNoTx: false,
		},

		// --- false-positive shapes that must NOT match ---
		{
			name:     "marker inside string literal",
			content:  "INSERT INTO docs(t) VALUES ('-- hasura:no-transaction');",
			wantNoTx: false,
		},
		{
			name:     "marker inside block comment negation",
			content:  "/* this migration does NOT use -- hasura:no-transaction */ SELECT 1;",
			wantNoTx: false,
		},
		{
			name:     "marker as trailing comment on a code line",
			content:  "SELECT 1; -- hasura:no-transaction",
			wantNoTx: false,
		},
		{
			name:     "marker with extra trailing text",
			content:  "-- hasura:no-transaction and some other text\nSELECT 1;",
			wantNoTx: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r := strings.NewReader(tt.content)
			gotNoTx, wrapped := detectNoTransactionComment(r)
			if gotNoTx != tt.wantNoTx {
				t.Errorf("noTx = %v, want %v", gotNoTx, tt.wantNoTx)
			}

			// Verify the returned reader still contains the full original content.
			got, err := io.ReadAll(wrapped)
			if err != nil {
				t.Fatalf("reading wrapped reader: %v", err)
			}
			if string(got) != tt.content {
				t.Errorf("wrapped reader content = %q, want %q", string(got), tt.content)
			}
		})
	}
}
