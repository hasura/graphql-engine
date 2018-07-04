package version

import "testing"

func TestGetConsoleTemplateVersion(t *testing.T) {
	tt := []struct {
		name   string
		server string
		tv     string
	}{
		{"untagged server version", "some-random-version", unversioned},
		{"no server version", "", preReleaseVersion},
		{"tagged server version", "v1.2.4", "v1.2"},
		{"tagged server version without v", "2.3.4", "v2.3"},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			v := &Version{}
			v.SetServerVersion(tc.server)
			tv := v.GetConsoleTemplateVersion()
			if tv != tc.tv {
				t.Fatalf("expected tv '%v', got '%v'", tc.tv, tv)
			}
		})
	}
}

func TestGetConsoleAssetsVersion(t *testing.T) {
	tt := []struct {
		name   string
		server string
		tv     string
	}{
		{"untagged server version", "some-random-version", "some-random-version"},
		{"no server version", "", preReleaseVersion},
		{"tagged server version", "v1.2.4", "v1.2"},
		{"tagged server version without v", "2.3.4", "v2.3"},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			v := &Version{}
			v.SetServerVersion(tc.server)
			tv := v.GetConsoleAssetsVersion()
			if tv != tc.tv {
				t.Fatalf("expected tv '%v', got '%v'", tc.tv, tv)
			}
		})
	}
}
