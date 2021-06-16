package stub

import (
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/migrate/source"
	st "github.com/hasura/graphql-engine/cli/v2/migrate/source/testing"
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
	err = m.Append(&source.Migration{Version: 1, Direction: source.Up})
	if err != nil {
		t.Fatal(err)
	}
	err = m.Append(&source.Migration{Version: 1, Direction: source.Down})
	if err != nil {
		t.Fatal(err)
	}
	err = m.Append(&source.Migration{Version: 1, Direction: source.MetaUp})
	if err != nil {
		t.Fatal(err)
	}
	err = m.Append(&source.Migration{Version: 1, Direction: source.MetaDown})
	if err != nil {
		t.Fatal(err)
	}
	err = m.Append(&source.Migration{Version: 3, Direction: source.Up})
	if err != nil {
		t.Fatal(err)
	}
	err = m.Append(&source.Migration{Version: 4, Direction: source.MetaUp})
	if err != nil {
		t.Fatal(err)
	}
	err = m.Append(&source.Migration{Version: 5, Direction: source.Down})
	if err != nil {
		t.Fatal(err)
	}
	err = m.Append(&source.Migration{Version: 6, Direction: source.MetaDown})
	if err != nil {
		t.Fatal(err)
	}
	err = m.Append(&source.Migration{Version: 8, Direction: source.Up})
	if err != nil {
		t.Fatal(err)
	}
	err = m.Append(&source.Migration{Version: 8, Direction: source.Down})
	if err != nil {
		t.Fatal(err)
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
