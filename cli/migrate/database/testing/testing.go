package testing

import (
	"bytes"
	"fmt"
	"testing"
	"time"

	"github.com/hasura/graphql-engine/cli/migrate/database"
)

func TestNilVersion(t *testing.T, d database.Driver) {
	v, _, err := d.Version()
	if err != nil {
		t.Fatal(err)
	}
	if v != database.NilVersion {
		t.Fatalf("Version: expected version to be NilVersion (-1), got %v", v)
	}
}

// Test runs tests against database implementations.
func Test(t *testing.T, d database.Driver) {
	TestLockAndUnlock(t, d)
	TestRunUp(t, d)
}

func TestLockAndUnlock(t *testing.T, d database.Driver) {
	// add a timeout, in case there is a deadlock
	done := make(chan bool, 1)
	go func() {
		timeout := time.After(15 * time.Second)
		for {
			select {
			case <-done:
				return
			case <-timeout:
				panic(fmt.Sprintf("Timeout after 15 seconds. Looks like a deadlock in Lock/UnLock.\n%#v", d))
			}
		}
	}()
	defer func() {
		done <- true
	}()

	// run the locking test ...

	if err := d.Lock(); err != nil {
		t.Fatal(err)
	}

	// try to acquire lock again
	if err := d.Lock(); err == nil {
		t.Fatal("Lock: expected err not to be nil")
	}

	// unlock
	if err := d.UnLock(); err != nil {
		t.Fatal(err)
	}

	// try to lock
	if err := d.Lock(); err != nil {
		t.Fatal(err)
	}
	if err := d.UnLock(); err != nil {
		t.Fatal(err)
	}
}

func TestRunUp(t *testing.T, d database.Driver) {
	tt := []struct {
		version       int64
		migrationData []byte
		migrationType string
		migrationFile string
	}{
		{version: 1, migrationData: []byte("/* foobar migration */"), migrationType: "sql"},
		{version: 3, migrationData: []byte("/* foobar migration */"), migrationType: "sql"},
	}

	for i, v := range tt {
		err := d.Run(bytes.NewReader(v.migrationData), v.migrationType, v.migrationFile)
		if err != nil {
			t.Fatalf("TestRun: expected err not to be nil in index %d", i)
		}

		err = d.InsertVersion(int64(v.version))
		if err != nil {
			t.Fatalf("TestRun-InsertVersion: expected err not to be nil in index %d", i)
		}

		currVersion, _, err := d.Version()
		if err != nil {
			t.Fatalf("TestRun-GetVersion: expected err not to be nil in index %d", i)
		}

		if currVersion != v.version {
			t.Fatalf("TestRun-MatchVersion: currVersion %d doesn't match with version %d in index %d", currVersion, v.version, i)
		}
	}
}
