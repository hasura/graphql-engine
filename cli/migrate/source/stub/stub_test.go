package stub

import (
	"testing"

	"github.com/hasura/graphql-engine/cli/migrate/source"
	st "github.com/hasura/graphql-engine/cli/migrate/source/testing"
	"github.com/sirupsen/logrus/hooks/test"
)

func Test(t *testing.T) {
	logger, _ := test.NewNullLogger()
	s := &Stub{}
	d, err := s.Open("", logger)
	if err != nil {
		t.Fatal(err)
	}

	m := source.NewMigrations()
	errUp := m.Append(&source.Migration{Version: 1, Direction: source.Up})
	if errUp != nil {
		t.Fatal(errUp)
	}
	errDown := m.Append(&source.Migration{Version: 1, Direction: source.Down})
	if errDown != nil {
		t.Fatal(errDown)
	}
	errMetaUp := m.Append(&source.Migration{Version: 1, Direction: source.MetaUp})
	if errMetaUp != nil {
		t.Fatal(errMetaUp)
	}
	errMetaDown := m.Append(&source.Migration{Version: 1, Direction: source.MetaDown})
	if errMetaDown != nil {
		t.Fatal(errMetaDown)
	}
	errUpVer3 := m.Append(&source.Migration{Version: 3, Direction: source.Up})
	if errUpVer3 != nil {
		t.Fatal(errUpVer3)
	}
	errMetaUpVer4 := m.Append(&source.Migration{Version: 4, Direction: source.MetaUp})
	if errMetaUpVer4 != nil {
		t.Fatal(errMetaUpVer4)
	}
	errDownVer5 := m.Append(&source.Migration{Version: 5, Direction: source.Down})
	if errDownVer5 != nil {
		t.Fatal(errDownVer5)
	}
	errMetaDownVer6 := m.Append(&source.Migration{Version: 6, Direction: source.MetaDown})
	if errMetaDownVer6 != nil {
		t.Fatal(errMetaDownVer6)
	}
	errUpVer8 := m.Append(&source.Migration{Version: 8, Direction: source.Up})
	if errUpVer8 != nil {
		t.Fatal(errUpVer8)
	}
	errDownVer8 := m.Append(&source.Migration{Version: 8, Direction: source.Down})
	if errDownVer8 != nil {
		t.Fatal(errDownVer8)
	}

	d.(*Stub).Migrations = m

	st.Test(t, d)
}

func TestWithEmptyMigration(t *testing.T) {
	logger, _ := test.NewNullLogger()
	s := &Stub{}
	d, err := s.Open("", logger)
	if err != nil {
		t.Fatal(err)
	}

	m := source.NewMigrations()

	d.(*Stub).Migrations = m

	version, err := d.First()
	if err == nil {
		t.Fatalf("First: expected err not to be nil")
	}

	if version != 0 {
		t.Errorf("First: expected 0, got %v", version)
	}
}
