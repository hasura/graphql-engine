package file

import (
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"testing"

	st "github.com/hasura/graphql-engine/cli/migrate/source/testing"
	"github.com/sirupsen/logrus/hooks/test"
)

func Test(t *testing.T) {
	tmpDir, err := ioutil.TempDir("", "")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	// write files that meet driver test requirements
	mustWriteFile(t, tmpDir, "1_foobar.up.sql", "1 up")
	mustWriteFile(t, tmpDir, "1_foobar.down.sql", "1 down")
	mustWriteFile(t, tmpDir, "1_foobar.up.yaml", `- args:
    name: test
  type: add_existing_table_or_view
`)
	mustWriteFile(t, tmpDir, "1_foobar.down.yaml", `- args:
    name: test
  type: add_existing_table_or_view
`)

	mustWriteFile(t, tmpDir, "3_foobar.up.sql", "3 up")

	mustWriteFile(t, tmpDir, "4_foobar.up.yaml", `- args:
    name: test
  type: add_existing_table_or_view
`)

	mustWriteFile(t, tmpDir, "5_foobar.down.sql", "5 down")

	mustWriteFile(t, tmpDir, "6_foobar.down.yaml", `- args:
    name: test
  type: add_existing_table_or_view
`)

	mustWriteFile(t, tmpDir, "8_foobar.up.sql", "7 up")
	mustWriteFile(t, tmpDir, "8_foobar.down.sql", "7 down")

	logger, _ := test.NewNullLogger()
	f := &File{}
	d, err := f.Open("file://"+tmpDir, logger)
	if err != nil {
		t.Fatal(err)
	}
	err = d.Scan()
	if err != nil {
		t.Fatal(err)
	}
	st.Test(t, d)
}

func TestOpen(t *testing.T) {
	tmpDir, err := ioutil.TempDir("", "TestOpen")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	mustWriteFile(t, tmpDir, "1_foobar.up.sql", "")
	mustWriteFile(t, tmpDir, "1_foobar.down.sql", "")

	if !filepath.IsAbs(tmpDir) {
		t.Fatal("expected tmpDir to be absolute path")
	}

	logger, _ := test.NewNullLogger()
	f := &File{}
	_, err = f.Open("file://"+tmpDir, logger) // absolute path
	if err != nil {
		t.Fatal(err)
	}
}

func TestOpenWithRelativePath(t *testing.T) {
	tmpDir, err := ioutil.TempDir("", "TestOpen")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	defer os.Chdir(wd) // rescue working dir after we are done

	if err := os.Chdir(tmpDir); err != nil {
		t.Fatal(err)
	}

	if err := os.Mkdir(filepath.Join(tmpDir, "foo"), os.ModePerm); err != nil {
		t.Fatal(err)
	}

	mustWriteFile(t, filepath.Join(tmpDir, "foo"), "1_foobar.up.sql", "test")

	logger, _ := test.NewNullLogger()
	f := &File{}

	// dir: foo
	d, err := f.Open("file://foo", logger)
	if err != nil {
		t.Fatal(err)
	}
	err = d.Scan()
	if err != nil {
		t.Fatal(err)
	}
	_, err = d.First()
	if err != nil {
		t.Fatalf("expected first file in working dir %v for foo", tmpDir)
	}

	// dir: ./foo
	d, err = f.Open("file://./foo", logger)
	if err != nil {
		t.Fatal(err)
	}
	err = d.Scan()
	if err != nil {
		t.Fatal(err)
	}
	_, err = d.First()
	if err != nil {
		t.Fatalf("expected first file in working dir %v for ./foo", tmpDir)
	}
}

func TestOpenDefaultsToCurrentDirectory(t *testing.T) {
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	logger, _ := test.NewNullLogger()
	f := &File{}
	d, err := f.Open("file://", logger)
	if err != nil {
		t.Fatal(err)
	}

	if d.(*File).path != wd {
		t.Fatal("expected driver to default to current directory")
	}
}

func TestOpenWithDuplicateVersion(t *testing.T) {
	tmpDir, err := ioutil.TempDir("", "TestOpenWithDuplicateVersion")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	mustWriteFile(t, tmpDir, "1_foo.up.sql", "test") // 1 up
	mustWriteFile(t, tmpDir, "1_bar.up.sql", "test") // 1 up

	logger, _ := test.NewNullLogger()
	f := &File{}
	d, err := f.Open("file://"+tmpDir, logger)
	if err != nil {
		t.Fatal("expected err not to be nil")
	}
	err = d.Scan()
	if err == nil {
		t.Fatal("expected err not to be nil")
	}
}

func TestOpenWithInvalidFileURL(t *testing.T) {
	logger, _ := test.NewNullLogger()
	f := &File{}
	_, err := f.Open(":", logger)
	if err == nil {
		t.Fatal("exepected err to be not nil")
	}
}

func TestWithEmptyMigration(t *testing.T) {
	logger, _ := test.NewNullLogger()
	s := &File{}
	d, err := s.Open("", logger)
	if err != nil {
		t.Fatal(err)
	}

	version, err := d.First()
	if err == nil {
		t.Fatalf("First: expected err not to be nil")
	}

	if version != 0 {
		t.Errorf("First: expected 0, got %v", version)
	}
}

func TestWithInvalidDirectory(t *testing.T) {
	logger, _ := test.NewNullLogger()
	s := &File{}
	_, err := s.Open("file://invalidir", logger)
	if err != nil {
		t.Fatal("exepected err to be nil")
	}
	err = s.Scan()
	if err == nil {
		t.Fatal("exepected err not to be nil")
	}
}

func TestClose(t *testing.T) {
	tmpDir, err := ioutil.TempDir("", "TestOpen")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(tmpDir)

	logger, _ := test.NewNullLogger()
	f := &File{}
	d, err := f.Open("file://"+tmpDir, logger)
	if err != nil {
		t.Fatal(err)
	}

	if d.Close() != nil {
		t.Fatal("expected nil")
	}
}

func mustWriteFile(t testing.TB, dir, file string, body string) {
	if err := ioutil.WriteFile(path.Join(dir, file), []byte(body), 06444); err != nil {
		t.Fatal(err)
	}
}
