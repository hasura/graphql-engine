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
		{"untagged server version", "some-random-version", "versioned/some-random-version"},
		{"no server version", "", preReleaseVersion},
		{"tagged server version", "v1.2.4", "channel/stable/v1.2"},
		{"tagged server version without v", "2.3.4", "channel/stable/v2.3"},
		{"tagged alpha release without .", "v1.0.0-alpha45", "channel/alpha/v1.0"},
		{"tagged beta release with .", "v1.0.0-beta.01", "channel/beta/v1.0"},
		{"tagged rc release with .", "v2.3.1-rc.11", "channel/rc/v2.3"},
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
