package source

import (
	"fmt"
	"sort"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

// Direction is either up or down.
type Direction string

const (
	Down     Direction = "down"
	Up       Direction = "up"
	MetaDown Direction = "metadown"
	MetaUp   Direction = "metaup"
)

// Migration is a helper struct for source drivers that need to
// build the full directory tree in memory.
// Migration is fully independent from migrate.Migration.
type Migration struct {
	// Version is the version of this migration.
	Version uint64

	// Identifier can be any string that helps identifying
	// this migration in the source.
	Identifier string

	// Direction is either Up or Down.
	Direction Direction

	// Raw holds the raw location path to this migration in source.
	// ReadUp and ReadDown will use this.
	Raw string

	// Check if the file exists in a directory
	IsDir bool
}

// Migrations wraps Migration and has an internal index
// to keep track of Migration order.
type Migrations struct {
	Index      uint64Slice
	Migrations map[uint64]map[Direction]*Migration
}

func NewMigrations() *Migrations {
	return &Migrations{
		Index:      make(uint64Slice, 0),
		Migrations: make(map[uint64]map[Direction]*Migration),
	}
}

func (i *Migrations) Append(m *Migration) (err error) {
	var op errors.Op = "source.Migrations.Append"
	if m == nil {
		return errors.E(op, fmt.Errorf("migration cannot be nill"))
	}

	if i.Migrations[m.Version] == nil {
		i.Migrations[m.Version] = make(map[Direction]*Migration)
	}

	// reject duplicate versions
	if migration, dup := i.Migrations[m.Version][m.Direction]; dup {
		return errors.E(op, fmt.Errorf("found duplicate migrations for version %d\n- %s\n- %s", m.Version, m.Raw, migration.Raw))
	}

	i.Migrations[m.Version][m.Direction] = m
	i.buildIndex()
	return nil
}

func (i *Migrations) buildIndex() {
	i.Index = make(uint64Slice, 0)
	for version := range i.Migrations {
		i.Index = append(i.Index, version)
	}
	sort.Sort(i.Index)
}

func (i *Migrations) First() (version uint64, ok bool) {
	if len(i.Index) == 0 {
		return 0, false
	}
	return i.Index[0], true
}

func (i *Migrations) GetLocalVersion() uint64 {
	if len(i.Index) == 0 {
		return 0
	}
	return i.Index[len(i.Index)-1]
}

func (i *Migrations) GetUnappliedMigrations(version uint64) (versions []uint64) {
	if version == 0 {
		return i.Index[0:]
	}
	pos := i.findPos(version)
	if pos >= 0 && len(i.Index) > pos+1 {
		return i.Index[pos+1:]
	}
	return versions
}

func (i *Migrations) Prev(version uint64) (prevVersion uint64, ok bool) {
	pos := i.findPos(version)
	if pos >= 1 && len(i.Index) > pos-1 {
		return i.Index[pos-1], true
	}
	return 0, false
}

func (i *Migrations) Next(version uint64) (nextVersion uint64, ok bool) {
	pos := i.findPos(version)
	if pos >= 0 && len(i.Index) > pos+1 {
		return i.Index[pos+1], true
	}
	return 0, false
}

func (i *Migrations) GetDirections(version uint64) map[Direction]bool {
	var directions = map[Direction]bool{
		Down:     false,
		Up:       false,
		MetaDown: false,
		MetaUp:   false,
	}
	for k := range i.Migrations[version] {
		directions[k] = true
	}
	return directions
}

func (i *Migrations) Up(version uint64) (m *Migration, ok bool) {
	if _, ok := i.Migrations[version]; ok {
		if mx, ok := i.Migrations[version][Up]; ok {
			return mx, true
		}
	}
	return nil, false
}

func (i *Migrations) MetaUp(version uint64) (m *Migration, ok bool) {
	if _, ok := i.Migrations[version]; ok {
		if mx, ok := i.Migrations[version][MetaUp]; ok {
			return mx, true
		}
	}
	return nil, false
}

func (i *Migrations) Down(version uint64) (m *Migration, ok bool) {
	if _, ok := i.Migrations[version]; ok {
		if mx, ok := i.Migrations[version][Down]; ok {
			return mx, true
		}
	}
	return nil, false
}

func (i *Migrations) MetaDown(version uint64) (m *Migration, ok bool) {
	if _, ok := i.Migrations[version]; ok {
		if mx, ok := i.Migrations[version][MetaDown]; ok {
			return mx, true
		}
	}
	return nil, false
}

func (i *Migrations) ReadName(version uint64) (name string) {
	for k := range i.Migrations[version] {
		return i.Migrations[version][k].Identifier
	}
	return "-"
}

func (i *Migrations) findPos(version uint64) int {
	if len(i.Index) > 0 {
		ix := i.Index.Search(version)
		if ix < len(i.Index) && i.Index[ix] == version {
			return ix
		}
	}
	return -1
}

type uint64Slice []uint64

func (s uint64Slice) Len() int {
	return len(s)
}

func (s uint64Slice) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

func (s uint64Slice) Less(i, j int) bool {
	return s[i] < s[j]
}

func (s uint64Slice) Search(x uint64) int {
	return sort.Search(len(s), func(i int) bool { return s[i] >= x })
}
