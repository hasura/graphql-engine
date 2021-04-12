package testing

import (
	"os"
	"testing"

	"github.com/hasura/graphql-engine/cli/migrate/source"
)

// Test runs tests against source implementations.
// It assumes that the driver tests has access to the following migrations:
//
// u = up migration, d = down migration, mu = meta up migration, md = meta down migration n = version
//  |  1        |  -  |  3  |  4  |  5  |  6   |  -  |  8  |
//  | u mu d md |  -  | u   |  mu |  d  |  md  |  -   | u d |
//
// See source/stub/stub_test.go or source/file/file_test.go for an example.

func Test(t *testing.T, d source.Driver) {
	TestFirst(t, d)
	TestNext(t, d)
	TestPrev(t, d)
	TestReadUp(t, d)
	TestReadDown(t, d)
	TestReadMetaUp(t, d)
	TestReadMetaDown(t, d)
	TestClose(t, d)
	TestGetDirections(t, d)
	TestGetLocalVersion(t, d)
	TestGetUnappliedMigrations(t, d)
}

func TestFirst(t *testing.T, d source.Driver) {
	version, err := d.First()
	if err != nil {
		t.Fatalf("First: expected err to be nil, got %v", err)
	}
	if version != 1 {
		t.Errorf("First: expected 1, got %v", version)
	}
}

func TestNext(t *testing.T, d source.Driver) {
	tt := []struct {
		version           uint64
		expectErr         error
		expectNextVersion uint64
	}{
		{version: 0, expectErr: os.ErrNotExist},
		{version: 1, expectErr: nil, expectNextVersion: 3},
		{version: 2, expectErr: os.ErrNotExist},
		{version: 3, expectErr: nil, expectNextVersion: 4},
		{version: 4, expectErr: nil, expectNextVersion: 5},
		{version: 5, expectErr: nil, expectNextVersion: 6},
		{version: 6, expectErr: nil, expectNextVersion: 8},
		{version: 7, expectErr: os.ErrNotExist},
		{version: 8, expectErr: os.ErrNotExist},
		{version: 9, expectErr: os.ErrNotExist},
	}

	for i, v := range tt {
		nv, err := d.Next(v.version)
		if (v.expectErr == os.ErrNotExist && !os.IsNotExist(err)) && v.expectErr != err {
			t.Errorf("Next: expected %v, got %v, in %v", v.expectErr, err, i)
		}
		if err == nil && v.expectNextVersion != nv {
			t.Errorf("Next: expected %v, got %v, in %v", v.expectNextVersion, nv, i)
		}
	}
}

func TestPrev(t *testing.T, d source.Driver) {
	tt := []struct {
		version           uint64
		expectErr         error
		expectPrevVersion uint64
	}{
		{version: 0, expectErr: os.ErrNotExist},
		{version: 1, expectErr: os.ErrNotExist},
		{version: 2, expectErr: os.ErrNotExist},
		{version: 3, expectErr: nil, expectPrevVersion: 1},
		{version: 4, expectErr: nil, expectPrevVersion: 3},
		{version: 5, expectErr: nil, expectPrevVersion: 4},
		{version: 6, expectErr: nil, expectPrevVersion: 5},
		{version: 7, expectErr: nil, expectPrevVersion: 5},
		{version: 8, expectErr: nil, expectPrevVersion: 6},
		{version: 9, expectErr: os.ErrNotExist},
	}

	for i, v := range tt {
		pv, err := d.Prev(v.version)
		if (v.expectErr == os.ErrNotExist && !os.IsNotExist(err)) && v.expectErr != err {
			t.Errorf("Prev: expected %v, got %v, in %v", v.expectErr, err, i)
		}
		if err == nil && v.expectPrevVersion != pv {
			t.Errorf("Prev: expected %v, got %v, in %v", v.expectPrevVersion, pv, i)
		}
	}
}

func TestReadUp(t *testing.T, d source.Driver) {
	tt := []struct {
		version   uint64
		expectErr error
		expectUp  bool
	}{
		{version: 0, expectErr: os.ErrNotExist},
		{version: 1, expectErr: nil, expectUp: true},
		{version: 2, expectErr: os.ErrNotExist},
		{version: 3, expectErr: nil, expectUp: true},
		{version: 4, expectErr: os.ErrNotExist},
		{version: 5, expectErr: os.ErrNotExist},
		{version: 6, expectErr: os.ErrNotExist},
		{version: 7, expectErr: os.ErrNotExist},
		{version: 8, expectErr: nil, expectUp: true},
	}

	for i, v := range tt {
		up, identifier, _, err := d.ReadUp(v.version)
		if (v.expectErr == os.ErrNotExist && !os.IsNotExist(err)) ||
			(v.expectErr != os.ErrNotExist && err != v.expectErr) {
			t.Errorf("expected %v, got %v, in %v", v.expectErr, err, i)

		} else if err == nil {
			if len(identifier) == 0 {
				t.Errorf("expected identifier not to be empty, in %v", i)
			}

			if v.expectUp == true && up == nil {
				t.Errorf("expected up not to be nil, in %v", i)
			} else if v.expectUp == false && up != nil {
				t.Errorf("expected up to be nil, got %v, in %v", up, i)
			}
		}
	}
}

func TestReadDown(t *testing.T, d source.Driver) {
	tt := []struct {
		version    uint64
		expectErr  error
		expectDown bool
	}{
		{version: 0, expectErr: os.ErrNotExist},
		{version: 1, expectErr: nil, expectDown: true},
		{version: 2, expectErr: os.ErrNotExist},
		{version: 3, expectErr: os.ErrNotExist},
		{version: 4, expectErr: os.ErrNotExist},
		{version: 5, expectErr: nil, expectDown: true},
		{version: 6, expectErr: os.ErrNotExist},
		{version: 7, expectErr: os.ErrNotExist},
		{version: 8, expectErr: nil, expectDown: true},
	}

	for i, v := range tt {
		down, identifier, _, err := d.ReadDown(v.version)
		if (v.expectErr == os.ErrNotExist && !os.IsNotExist(err)) ||
			(v.expectErr != os.ErrNotExist && err != v.expectErr) {
			t.Errorf("expected %v, got %v, in %v", v.expectErr, err, i)

		} else if err == nil {
			if len(identifier) == 0 {
				t.Errorf("expected identifier not to be empty, in %v", i)
			}

			if v.expectDown == true && down == nil {
				t.Errorf("expected down not to be nil, in %v", i)
			} else if v.expectDown == false && down != nil {
				t.Errorf("expected down to be nil, got %v, in %v", down, i)
			}
		}
	}
}

func TestReadMetaUp(t *testing.T, d source.Driver) {
	tt := []struct {
		version      uint64
		expectErr    error
		expectMetaUp bool
	}{
		{version: 0, expectErr: os.ErrNotExist},
		{version: 1, expectErr: nil, expectMetaUp: true},
		{version: 2, expectErr: os.ErrNotExist},
		{version: 3, expectErr: os.ErrNotExist},
		{version: 4, expectErr: nil, expectMetaUp: true},
		{version: 5, expectErr: os.ErrNotExist},
		{version: 6, expectErr: os.ErrNotExist},
		{version: 7, expectErr: os.ErrNotExist},
		{version: 8, expectErr: os.ErrNotExist},
	}

	for i, v := range tt {
		up, identifier, _, err := d.ReadMetaUp(v.version)
		if (v.expectErr == os.ErrNotExist && !os.IsNotExist(err)) ||
			(v.expectErr != os.ErrNotExist && err != v.expectErr) {
			t.Errorf("expected %v, got %v, in %v", v.expectErr, err, i)

		} else if err == nil {
			if len(identifier) == 0 {
				t.Errorf("expected identifier not to be empty, in %v", i)
			}

			if v.expectMetaUp == true && up == nil {
				t.Errorf("expected up not to be nil, in %v", i)
			} else if v.expectMetaUp == false && up != nil {
				t.Errorf("expected up to be nil, got %v, in %v", up, i)
			}
		}
	}
}

func TestReadMetaDown(t *testing.T, d source.Driver) {
	tt := []struct {
		version        uint64
		expectErr      error
		expectMetaDown bool
	}{
		{version: 0, expectErr: os.ErrNotExist},
		{version: 1, expectErr: nil, expectMetaDown: true},
		{version: 2, expectErr: os.ErrNotExist},
		{version: 3, expectErr: os.ErrNotExist},
		{version: 4, expectErr: os.ErrNotExist},
		{version: 5, expectErr: os.ErrNotExist},
		{version: 6, expectErr: nil, expectMetaDown: true},
		{version: 7, expectErr: os.ErrNotExist},
		{version: 8, expectErr: os.ErrNotExist},
	}

	for i, v := range tt {
		up, identifier, _, err := d.ReadMetaDown(v.version)
		if (v.expectErr == os.ErrNotExist && !os.IsNotExist(err)) ||
			(v.expectErr != os.ErrNotExist && err != v.expectErr) {
			t.Errorf("expected %v, got %v, in %v", v.expectErr, err, i)

		} else if err == nil {
			if len(identifier) == 0 {
				t.Errorf("expected identifier not to be empty, in %v", i)
			}

			if v.expectMetaDown == true && up == nil {
				t.Errorf("expected up not to be nil, in %v", i)
			} else if v.expectMetaDown == false && up != nil {
				t.Errorf("expected up to be nil, got %v, in %v", up, i)
			}
		}
	}
}

func TestClose(t *testing.T, d source.Driver) {
	err := d.Close()
	if err != nil {
		t.Fatalf("Close: expected err not to be nil")
	}
}

func TestGetDirections(t *testing.T, d source.Driver) {
	tt := []struct {
		version        uint64
		expectUp       bool
		expectDown     bool
		expectMetaUp   bool
		expectMetaDown bool
	}{
		{version: 0},
		{version: 1, expectUp: true, expectDown: true, expectMetaUp: true, expectMetaDown: true},
		{version: 2},
		{version: 3, expectUp: true},
		{version: 4, expectMetaUp: true},
		{version: 5, expectDown: true},
		{version: 6, expectMetaDown: true},
		{version: 7},
		{version: 8, expectUp: true, expectDown: true},
	}

	for _, v := range tt {
		directions := d.GetDirections(v.version)
		if directions[source.Up] != v.expectUp {
			t.Fatalf("expected up direction to be %t but got %t", v.expectUp, directions[source.Up])
		}

		if directions[source.Down] != v.expectDown {
			t.Fatalf("expected down direction to be %t but got %t", v.expectDown, directions[source.Down])
		}

		if directions[source.MetaUp] != v.expectMetaUp {
			t.Fatalf("expected meta up direction to be %t but got %t", v.expectMetaUp, directions[source.MetaUp])
		}

		if directions[source.MetaDown] != v.expectMetaDown {
			t.Fatalf("expected meta down direction to be %t but got %t", v.expectMetaDown, directions[source.MetaDown])
		}
	}
}

func TestGetLocalVersion(t *testing.T, d source.Driver) {
	expectedLocalVersion := uint64(8)
	version, err := d.GetLocalVersion()
	if err != nil {
		t.Fatalf("GetLocalVersion: expected err not to be nil")
	}

	if version != expectedLocalVersion {
		t.Fatalf("GetLocalVersion: expected version to be %d, but got %d", expectedLocalVersion, version)
	}
}

func TestGetUnappliedMigrations(t *testing.T, d source.Driver) {
	tt := []struct {
		version            uint64
		expectedMigrations []uint64
	}{
		{version: 0, expectedMigrations: []uint64{1, 3, 4, 5, 6, 8}},
		{version: 1, expectedMigrations: []uint64{3, 4, 5, 6, 8}},
		{version: 3, expectedMigrations: []uint64{4, 5, 6, 8}},
		{version: 4, expectedMigrations: []uint64{5, 6, 8}},
		{version: 5, expectedMigrations: []uint64{6, 8}},
		{version: 6, expectedMigrations: []uint64{8}},
		{version: 8, expectedMigrations: []uint64{}},
	}

	for index, v := range tt {
		versions := d.GetUnappliedMigrations(v.version)
		if len(versions) != len(v.expectedMigrations) {
			t.Fatalf("GetUnappliedMigrations: expected length to be %d but got %d", len(v.expectedMigrations), len(versions))
		}

		for i, v := range v.expectedMigrations {
			if v != versions[i] {
				t.Fatalf("GetUnappliedMigrations: expected version to be %d but got %d in index %d in %d", v, versions[i], i, index)
			}
		}
	}
}
