package testing

import (
	"errors"
	"os"
	"testing"

	internalerrors "github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/migrate/source"
	"github.com/stretchr/testify/require"
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
		wantErr           bool
		expectNextVersion uint64
		assertErr         require.ErrorAssertionFunc
	}{
		{
			version: 0,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version:           1,
			wantErr:           false,
			expectNextVersion: 3,
			assertErr:         require.NoError,
		},
		{
			version: 2,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version:           3,
			wantErr:           false,
			expectNextVersion: 4,
			assertErr:         require.NoError,
		},
		{
			version:           4,
			wantErr:           false,
			expectNextVersion: 5,
			assertErr:         require.NoError,
		},
		{
			version:           5,
			wantErr:           false,
			expectNextVersion: 6,
			assertErr:         require.NoError,
		},
		{
			version:           6,
			wantErr:           false,
			expectNextVersion: 8,
			assertErr:         require.NoError,
		},
		{
			version: 7,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 8,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 9,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
	}

	for i, v := range tt {
		nv, err := d.Next(v.version)
		v.assertErr(t, err)
		if v.wantErr {
			return
		}
		if v.expectNextVersion != nv {
			t.Errorf("Next: expected %v, got %v, in %v", v.expectNextVersion, nv, i)
		}
	}
}

func TestPrev(t *testing.T, d source.Driver) {
	tt := []struct {
		version           uint64
		wantErr           bool
		expectPrevVersion uint64
		assertErr         require.ErrorAssertionFunc
	}{
		{
			version: 0,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 1,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 2,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version:           3,
			wantErr:           false,
			expectPrevVersion: 1,
			assertErr:         require.NoError,
		},
		{
			version:           4,
			wantErr:           false,
			expectPrevVersion: 3,
			assertErr:         require.NoError,
		},
		{
			version:           5,
			wantErr:           false,
			expectPrevVersion: 4,
			assertErr:         require.NoError,
		},
		{
			version:           6,
			wantErr:           false,
			expectPrevVersion: 5,
			assertErr:         require.NoError,
		},
		{
			version:           7,
			wantErr:           false,
			expectPrevVersion: 5,
			assertErr:         require.NoError,
		},
		{
			version:           8,
			wantErr:           false,
			expectPrevVersion: 6,
			assertErr:         require.NoError,
		},
		{
			version: 9,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
	}

	for i, v := range tt {
		pv, err := d.Prev(v.version)
		v.assertErr(t, err)
		if v.wantErr {
			return
		}
		if v.expectPrevVersion != pv {
			t.Errorf("Prev: expected %v, got %v, in %v", v.expectPrevVersion, pv, i)
		}
	}
}

func TestReadUp(t *testing.T, d source.Driver) {
	tt := []struct {
		version   uint64
		wantErr   bool
		expectUp  bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			version: 0,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version:   1,
			wantErr:   false,
			expectUp:  true,
			assertErr: require.NoError,
		},
		{
			version: 2,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version:   3,
			wantErr:   false,
			expectUp:  true,
			assertErr: require.NoError,
		},
		{
			version: 4,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 5,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 6,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 7,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version:   8,
			wantErr:   false,
			expectUp:  true,
			assertErr: require.NoError,
		},
	}

	for i, v := range tt {
		up, identifier, _, err := d.ReadUp(v.version)
		v.assertErr(t, err)
		if v.wantErr {
			return
		}
		if len(identifier) == 0 {
			t.Errorf("expected identifier not to be empty, in %v", i)
		}

		if v.expectUp && up == nil {
			t.Errorf("expected up not to be nil, in %v", i)
		} else if !v.expectUp && up != nil {
			t.Errorf("expected up to be nil, got %v, in %v", up, i)
		}
	}
}

func TestReadDown(t *testing.T, d source.Driver) {
	tt := []struct {
		version    uint64
		wantErr    bool
		expectDown bool
		assertErr  require.ErrorAssertionFunc
	}{
		{
			version: 0,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version:    1,
			wantErr:    false,
			expectDown: true,
			assertErr:  require.NoError,
		},
		{
			version: 2,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 3,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 4,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version:    5,
			wantErr:    false,
			expectDown: true,
			assertErr:  require.NoError,
		},
		{
			version: 6,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 7,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version:    8,
			wantErr:    false,
			expectDown: true,
			assertErr:  require.NoError,
		},
	}

	for i, v := range tt {
		down, identifier, _, err := d.ReadDown(v.version)
		v.assertErr(t, err)
		if v.wantErr {
			return
		}
		if len(identifier) == 0 {
			t.Errorf("expected identifier not to be empty, in %v", i)
		}

		if v.expectDown && down == nil {
			t.Errorf("expected down not to be nil, in %v", i)
		} else if !v.expectDown && down != nil {
			t.Errorf("expected down to be nil, got %v, in %v", down, i)
		}
	}
}

func TestReadMetaUp(t *testing.T, d source.Driver) {
	tt := []struct {
		version      uint64
		wantErr      bool
		expectMetaUp bool
		assertErr    require.ErrorAssertionFunc
	}{
		{
			version: 0,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version:      1,
			wantErr:      false,
			expectMetaUp: true,
			assertErr:    require.NoError,
		},
		{
			version: 2,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 3,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version:      4,
			wantErr:      false,
			expectMetaUp: true,
			assertErr:    require.NoError,
		},
		{
			version: 5,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 6,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 7,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 8,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
	}

	for i, v := range tt {
		up, identifier, _, err := d.ReadMetaUp(v.version)
		v.assertErr(t, err)
		if v.wantErr {
			return
		}
		if len(identifier) == 0 {
			t.Errorf("expected identifier not to be empty, in %v", i)
		}

		if v.expectMetaUp && up == nil {
			t.Errorf("expected up not to be nil, in %v", i)
		} else if !v.expectMetaUp && up != nil {
			t.Errorf("expected up to be nil, got %v, in %v", up, i)
		}
	}
}

func TestReadMetaDown(t *testing.T, d source.Driver) {
	tt := []struct {
		version        uint64
		wantErr        bool
		expectMetaDown bool
		assertErr      require.ErrorAssertionFunc
	}{
		{
			version: 0,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version:        1,
			wantErr:        false,
			expectMetaDown: true,
			assertErr:      require.NoError,
		},
		{
			version: 2,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 3,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 4,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 5,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version:        6,
			wantErr:        false,
			expectMetaDown: true,
			assertErr:      require.NoError,
		},
		{
			version: 7,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
		{
			version: 8,
			wantErr: true,
			assertErr: require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &internalerrors.Error{}, err)
				require.True(t, errors.Is(err, os.ErrNotExist))
			}),
		},
	}

	for i, v := range tt {
		up, identifier, _, err := d.ReadMetaDown(v.version)
		v.assertErr(t, err)
		if v.wantErr {
			return
		}
		if len(identifier) == 0 {
			t.Errorf("expected identifier not to be empty, in %v", i)
		}

		if v.expectMetaDown && up == nil {
			t.Errorf("expected up not to be nil, in %v", i)
		} else if !v.expectMetaDown && up != nil {
			t.Errorf("expected up to be nil, got %v, in %v", up, i)
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
