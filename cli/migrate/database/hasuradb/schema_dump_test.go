package hasuradb

import "testing"

func TestQuoteSchemaName(t *testing.T) {
	tests := []struct {
		name string
		want string
	}{
		{"public", `"public"`},
		{"TEST", `"TEST"`},
		{"MySchema", `"MySchema"`},
		{`"TEST"`, `"TEST"`},
		{`we"ird`, `"we""ird"`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := quoteSchemaName(tc.name); got != tc.want {
				t.Errorf("quoteSchemaName(%q) = %q, want %q", tc.name, got, tc.want)
			}
		})
	}
}
