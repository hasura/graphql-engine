package util

import (
	"os"
	"testing"
)

var i = &InstallManifestsRepo{
	Namespace: "hasura",
	Name:      "graphql-engine-install-manifests",
}

func TestZipURL(t *testing.T) {
	expected := "https://github.com/hasura/graphql-engine-install-manifests/archive/master.zip"
	u := i.ZipURL()
	if u != expected {
		t.Fatalf("expected: %s, got %s", expected, u)
	}
}

func TestZipExtractedDirectory(t *testing.T) {
	expected := "graphql-engine-install-manifests-master"
	got := i.ZipExtractedDirectory()
	if got != expected {
		t.Fatalf("expected: %s, got %s", expected, got)
	}
}

func TestDownload(t *testing.T) {
	dir, err := i.Download()
	if err != nil {
		t.Fatalf("download failed: %v", err)
	}
	os.RemoveAll(dir)
}
