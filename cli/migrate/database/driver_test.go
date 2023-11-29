package database

import (
	"io"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/require"
)

type mockDriver struct {
	url string
}

func (m *mockDriver) Open(url string, isCmd bool, logger *logrus.Logger, hasuraOpts *HasuraOpts) (Driver, error) {
	return &mockDriver{
		url: url,
	}, nil
}

func (m *mockDriver) Close() error {
	return nil
}

func (m *mockDriver) UnLock() error {
	return nil
}

func (m *mockDriver) Run(migration io.Reader, fileType, fileName string) error {
	return nil
}
func (m *mockDriver) Lock() error {
	return nil
}

func (m *mockDriver) Scan() error {
	return nil
}

func (m *mockDriver) SetVersion(version int64, dirty bool) error {
	return nil
}

func (m *mockDriver) Version() (version int64, dirty bool, err error) {
	return 0, false, nil
}

func (m *mockDriver) Drop() error {
	return nil
}

func (m *mockDriver) InsertVersion(version int64) error {
	return nil
}

func (m *mockDriver) RemoveVersion(version int64) error {
	return nil
}

func (m *mockDriver) First() (migrationVersion *MigrationVersion, ok bool) {
	return nil, false
}

func (m *mockDriver) Last() (*MigrationVersion, bool) {
	return nil, false
}

func (m *mockDriver) Next(version uint64) (migrationVersion *MigrationVersion, ok bool) {
	return nil, false
}

func (m *mockDriver) Prev(version uint64) (prevVersion *MigrationVersion, ok bool) {
	return nil, false
}

func (m *mockDriver) Read(version uint64) (ok bool) {
	return false
}

func (m *mockDriver) PushToList(migration io.Reader, fileType string, list *CustomList) error {
	return nil
}

func (m *mockDriver) Query(data interface{}) error {
	return nil
}

func (m *mockDriver) ResetQuery() {
}

func (m *mockDriver) Squash(list *CustomList, ret chan<- interface{}) {
}

func (m *mockDriver) EnableCheckMetadataConsistency(enabled bool) {
}

func (m *mockDriver) ExportSchemaDump(includeSchemas []string, excludeSchemas []string, sourceName string, sourceKind hasura.SourceKind) ([]byte, error) {
	return nil, nil
}

func (m *mockDriver) GetSetting(name string) (value string, err error) {
	return "", nil
}

func (m *mockDriver) UpdateSetting(name string, value string) error {
	return nil
}

func (m *mockDriver) ApplySeed(interface{}) error {
	return nil
}
func (m *mockDriver) ExportDataDump(tableNames []string, sourceName string, sourceKind hasura.SourceKind) ([]byte, error) {
	return nil, nil
}

func TestRegisterTwice(t *testing.T) {
	Register("mock", &mockDriver{})

	var err interface{}
	func() {
		defer func() {
			err = recover()
		}()
		Register("mock", &mockDriver{})
	}()

	if err == nil {
		t.Fatal("expected a panic when calling Register twice")
	}
}

func TestOpen(t *testing.T) {
	func() {
		defer func() {
			_ = recover()
		}()
		Register("mock", &mockDriver{})
	}()

	cases := []struct {
		url       string
		wantErr   bool
		assertErr require.ErrorAssertionFunc
	}{
		{
			"mock://user:pass@host:1337/db",
			false,
			require.NoError,
		},
		{
			"unknown://bla",
			true,
			require.ErrorAssertionFunc(func(tt require.TestingT, err error, i ...interface{}) {
				require.IsType(t, &errors.Error{}, err)
				e := err.(*errors.Error)
				require.Equal(t, errors.Op("database.Open"), e.Op)
				require.Equal(t, "database driver: unknown driver unknown", e.Err.Error())
			}),
		},
	}

	for _, c := range cases {
		t.Run(c.url, func(t *testing.T) {
			d, err := Open(c.url, false, nil, nil)
			c.assertErr(t, err)
			if c.wantErr {
				return
			}
			if md, ok := d.(*mockDriver); !ok {
				t.Fatalf("expected *mockDriver got %T", d)
			} else if md.url != c.url {
				t.Fatalf("expected %q got %q", c.url, md.url)
			}
		})
	}
}
