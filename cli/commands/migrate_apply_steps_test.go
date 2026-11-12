package commands

import (
	"strings"
	"testing"
)

func TestGetMigrationTypeAndStep_RejectsVersionAsStepCount(t *testing.T) {
	tests := []struct {
		name          string
		up, down      string
		wantType      string
		wantStep      int64
		wantErrSubstr string
	}{
		{
			name:     "small --down count",
			down:     "2",
			wantType: "down",
			wantStep: 2,
		},
		{
			name:     "small --up count",
			up:       "5",
			wantType: "up",
			wantStep: 5,
		},
		{
			name:     "largest accepted --down count",
			down:     "999999999",
			wantType: "down",
			wantStep: 999999999,
		},
		{
			name:          "smallest rejected --up count",
			up:            "1000000000",
			wantErrSubstr: "--version 1000000000 --type up",
		},
		{
			name:          "--down given a millisecond version",
			down:          "1659058116550",
			wantErrSubstr: "--version 1659058116550 --type down",
		},
		{
			name:          "--up given a second version",
			up:            "1659058116",
			wantErrSubstr: "--version 1659058116 --type up",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotType, gotStep, err := getMigrationTypeAndStep(tt.up, tt.down, "", "", "", false)
			if tt.wantErrSubstr != "" {
				if err == nil {
					t.Fatalf("expected an error, got type %q step %d", gotType, gotStep)
				}
				if !strings.Contains(err.Error(), tt.wantErrSubstr) {
					t.Errorf("error %q does not contain %q", err.Error(), tt.wantErrSubstr)
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if gotType != tt.wantType || gotStep != tt.wantStep {
				t.Errorf("got (%q, %d), want (%q, %d)", gotType, gotStep, tt.wantType, tt.wantStep)
			}
		})
	}
}
