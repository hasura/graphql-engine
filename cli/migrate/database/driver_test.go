package database

import (
	"crypto/tls"
	"io"
	"testing"

	"github.com/hasura/graphql-engine/cli/internal/hasura"

	"github.com/sirupsen/logrus"
)

type mockDriver struct {
	url string
}

func (m *mockDriver) Open(url string, isCmd bool, tlsConfig *tls.Config, logger *logrus.Logger, hasuraOpts *HasuraOpts) (Driver, error) {
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
	return
}

func (m *mockDriver) Squash(list *CustomList, ret chan<- interface{}) {
	return
}

func (m *mockDriver) EnableCheckMetadataConsistency(enabled bool) {
}

func (m *mockDriver) ExportSchemaDump(schemaName []string, sourceName string, sourceKind hasura.SourceKind) ([]byte, error) {
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
		url string
		err bool
	}{
		{
			"mock://user:pass@host:1337/db",
			false,
		},
		{
			"unknown://bla",
			true,
		},
	}

	for _, c := range cases {
		t.Run(c.url, func(t *testing.T) {
			d, err := Open(c.url, false, nil, nil, nil)

			if err == nil {
				if c.err {
					t.Fatal("expected an error for an unknown driver")
				} else {
					if md, ok := d.(*mockDriver); !ok {
						t.Fatalf("expected *mockDriver got %T", d)
					} else if md.url != c.url {
						t.Fatalf("expected %q got %q", c.url, md.url)
					}
				}
			} else if !c.err {
				t.Fatalf("did not expect %q", err)
			}
		})
	}
}
