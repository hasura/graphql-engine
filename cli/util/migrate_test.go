package util

import (
	"io/ioutil"
	"net/url"
	"os"
	"testing"
)

func TestGetValidStepFromString(t *testing.T) {
	tt := []struct {
		step   string
		output int64
	}{
		{step: "1", output: 1},
		{step: "-1", output: -1},
	}

	for i, v := range tt {
		step, err := GetValidStepFromString(v.step)
		if err != nil {
			t.Fatal(err)
		}

		if step != v.output {
			t.Fatalf("expected step to be %d in index %d but got %d", v.output, i, step)
		}
	}
}

func TestGetDataPath(t *testing.T) {
	nurl := &url.URL{
		Scheme: "http",
		Host:   "localhost:8080",
	}
	expectedURL := "hasuradb://admin:@localhost:8080?sslmode=disable"
	dURL := GetDataPath(nurl, "")

	if dURL.String() != expectedURL {
		t.Fatalf("expected url to be %s, but got %s", expectedURL, dURL.String())
	}
}

func TestGetFilePath(t *testing.T) {
	// Create migration Dir
	migrationsDir, err := ioutil.TempDir("", "")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(migrationsDir)
	expectedURL := "file://" + migrationsDir

	fURL := GetFilePath(migrationsDir)
	if fURL.String() != expectedURL {
		t.Fatalf("expected url to be %s, but got %s", expectedURL, fURL.String())
	}
}
