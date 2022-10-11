package console

import (
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/version"
)

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
			v := &version.Version{}
			v.SetServerVersion(tc.server)
			templateProvider := NewDefaultTemplateProvider("test", "test", ConsoleFS)
			tv := templateProvider.GetTemplateVersion(v)
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
		{"tagged rj release with .", "v1.2.0-rj.1", "channel/rj/v1.2"},
		{"tagged cloud stable", "v2.0.0-cloud.9", "channel/stable/v2.0"},
		{"tagged pro stable", "v2.0.0-pro.9", "channel/stable/v2.0"},
		{"tagged pro alpha", "v2.0.0-alpha.pro.9", "channel/alpha/v2.0"},
		{"tagged cloud alpha", "v2.0.0-alpha.cloud.9", "channel/alpha/v2.0"},

		{"tagged ce stable version", "v2.12.0-ce", "channel/versioned/v2.12.0-ce"},
		{"tagged ce patch version", "v2.12.1-ce", "channel/versioned/v2.12.1-ce"},
		{"tagged ce first beta", "v2.12.0-beta.1-ce", "channel/versioned/v2.12.0-beta.1-ce"},
		{"tagged ce incremental beta", "v2.12.0-beta.2-ce", "channel/versioned/v2.12.0-beta.2-ce"},

		{"tagged ee lite stable version", "v2.12.0", "channel/stable/v2.12"},
		{"tagged ee lite patch version", "v2.12.1", "channel/stable/v2.12"},
		{"tagged ee lite first beta", "v2.12.0-beta.1", "channel/beta/v2.12"},
		{"tagged ee lite incremental beta", "v2.12.0-beta.2", "channel/beta/v2.12"},
		{"tagged ee lite first alpha", "v2.12.0-alpha.1", "channel/alpha/v2.12"},
		{"tagged ee lite incremental alpha", "v2.12.0-alpha.2", "channel/alpha/v2.12"},

		{"tagged cloud first stable", "v2.12.0-cloud.1", "channel/stable/v2.12"},
		{"tagged cloud incremental stable", "v2.12.0-cloud.2", "channel/stable/v2.12"},
		{"tagged cloud first patch", "v2.12.1-cloud.1", "channel/stable/v2.12"},
		{"tagged cloud incremental patch", "v2.12.1-cloud.2", "channel/stable/v2.12"},
		{"tagged cloud first beta", "v2.12.0-beta.1-cloud.1", "channel/beta/v2.12"},
		{"tagged cloud incremental beta", "v2.12.0-beta.2-cloud.1", "channel/beta/v2.12"},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			v := &version.Version{}
			v.SetServerVersion(tc.server)
			templateProvider := NewDefaultTemplateProvider("test", "test", ConsoleFS)
			tv := templateProvider.GetAssetsVersion(v)
			if tv != tc.tv {
				t.Fatalf("expected tv '%v', got '%v'", tc.tv, tv)
			}
		})
	}
}
