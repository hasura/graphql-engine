package version

import (
	"testing"
)

func TestNewCLIVersionWithSemver(t *testing.T) {
	v := NewCLIVersion("v1.0.0")
	if v == nil {
		t.Fatal("expected a version object, got nil")
	}
	if v.CLI != "v1.0.0" {
		t.Fatalf("expected v1.0.0, got %s", v.CLI)
	}
}

func TestNewCLIVersionWithDev(t *testing.T) {
	v := NewCLIVersion("dev")
	if v == nil {
		t.Fatal("expected a version object, got nil")
	}
	if v.CLI != "dev" {
		t.Fatalf("expected %s, got %s", "dev", v.CLI)
	}
}

func TestSetVersions(t *testing.T) {
	var one int64 = 1
	var zero int64 = 0
	tt := []struct {
		name  string
		in    string
		out   string
		major *int64
		minor *int64
	}{
		{"valid semver without v", "1.0.1-alpha01", "v1.0.1-alpha01", &one, &zero},
		{"valid semver with v", "v1.0.1-alpha01", "v1.0.1-alpha01", &one, &zero},
		{"invalid semver", "dev", "dev", nil, nil},
		{"invalid semver", "build-system-1234abc", "build-system-1234abc", nil, nil},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			v := &Version{}
			v.SetCLIVersion(tc.in)
			v.SetServerVersion(tc.in)
			if v.CLI != tc.out {
				t.Fatalf("expected version to be %s, got %s", tc.out, v.CLI)
			}
			if v.Server != tc.out {
				t.Fatalf("expected version to be %s, got %s", tc.out, v.Server)
			}
			if v.CLISemver == nil {
				if tc.major != nil {
					t.Fatalf("expected semver to parse, but did not")
				}
			} else {
				if v.CLISemver.Major() != *(tc.major) {
					t.Fatalf("expected major to be %d, got %d", *(tc.major), v.CLISemver.Major())
				}
			}
			if v.ServerSemver == nil {
				if tc.major != nil {
					t.Fatalf("expected semver to parse, but did not")
				}
			} else {
				if v.ServerSemver.Major() != *(tc.major) {
					t.Fatalf("expected major to be %d, got %d", *(tc.major), v.ServerSemver.Major())
				}
			}
		})
	}
}
