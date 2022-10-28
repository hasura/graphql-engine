package version

import "testing"

func TestCheckCLIServerCompatibility(t *testing.T) {
	tt := []struct {
		name       string
		cli        string
		server     string
		compatible bool
		reason     string
	}{
		{"untagged version match", "some-random-version", "some-random-version", true, untaggedBuild},
		{"untagged version mismatch", "some-random-version", "some-other-random-version", false, untaggedBuild},
		{"cli tagged server untagged", "v2.1.2", "some-random-version", false, untaggedBuild},
		{"no server version", "v2.1.8", "", true, noServerVersion},
		{"no cli and server version", "", "", false, noCLIVersion},
		{"tagged version exact match", "v1.2.4", "v1.2.4", true, compatibleCLIAndServer},
		{"tagged version major.minor match", "v1.3.4", "v1.3.1", true, compatibleCLIAndServer},
		{"tagged version with server pre-release, major.minor match", "v1.3.4", "v1.3.4-alpha13", true, compatibleCLIAndServer},
		{"tagged version with cli pre-release major.minor match", "v1.3.4-alpha13", "v1.3.4", true, compatibleCLIAndServer},
		{"tagged version with cli and server pre-release major.minor match", "v1.3.4-alpha13", "v1.3.4-alpha13", true, compatibleCLIAndServer},
		{"tagged version with cli and server pre-release major.minor match", "v1.3.1-alpha13", "v1.3.4-alpha16", true, compatibleCLIAndServer},
		{"tagged version major.minor mismatch, server ahead", "v2.1.8", "v7.2.9", false, olderCLIVersion},
		{"tagged version major.minor mismatch, server behind, different major", "v2.1.8", "v1.1.1", false, versionMismatch},
		{"tagged version major.minor mismatch, server behind, same major", "v2.1.8", "v2.0.1", true, versionMismatch},
		{"cli untagged server tagged", "some-random-version", "v1.0.1", true, untaggedCLI},
		{"no cli version", "", "v1.0.0", false, noCLIVersion},
		{"cloud server and matching CLI", "v2.1.8", "v2.1.1-cloud.1", true, compatibleCLIAndServer},
		{"cloud server and mismatched CLI minor", "v2.1.8", "v2.0.1-cloud.1", true, versionMismatch},
	}

	for _, tc := range tt {
		t.Run(tc.name, func(t *testing.T) {
			v := &Version{}
			v.SetCLIVersion(tc.cli)
			v.SetServerVersion(tc.server)
			c, r := v.CheckCLIServerCompatibility()
			if c != tc.compatible {
				t.Fatalf("expected compatibilty '%v', got '%v'", tc.compatible, c)
			}
			if r != tc.reason {
				t.Fatalf("expected reason '%v', got '%v'", tc.reason, r)
			}
		})
	}
}
