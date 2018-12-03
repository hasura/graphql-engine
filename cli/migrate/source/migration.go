package source

import (
	"fmt"
	"sort"
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
}

// Migrations wraps Migration and has an internal index
// to keep track of Migration order.
type Migrations struct {
	index      uint64Slice
	migrations map[uint64]map[Direction]*Migration
}

func NewMigrations() *Migrations {
	return &Migrations{
		index:      make(uint64Slice, 0),
		migrations: make(map[uint64]map[Direction]*Migration),
	}
}

func (i *Migrations) Append(m *Migration) (err error) {
	if m == nil {
		return fmt.Errorf("migration cannot be nill")
	}

	if i.migrations[m.Version] == nil {
		i.migrations[m.Version] = make(map[Direction]*Migration)
	}

	// reject duplicate versions
	if migration, dup := i.migrations[m.Version][m.Direction]; dup {
		return fmt.Errorf("found duplicate migrations for version %d\n- %s\n- %s", m.Version, m.Raw, migration.Raw)
	}

	i.migrations[m.Version][m.Direction] = m
	i.buildIndex()
	return nil
}

func (i *Migrations) buildIndex() {
	i.index = make(uint64Slice, 0)
	for version := range i.migrations {
		i.index = append(i.index, version)
	}
	sort.Sort(i.index)
}

func (i *Migrations) First() (version uint64, ok bool) {
	if len(i.index) == 0 {
		return 0, false
	}
	return i.index[0], true
}

func (i *Migrations) GetLocalVersion() uint64 {
	if len(i.index) == 0 {
		return 0
	}
	return i.index[len(i.index)-1]
}

func (i *Migrations) GetUnappliedMigrations(version uint64) (versions []uint64) {
	if version == 0 {
		return i.index[0:]
	}
	pos := i.findPos(version)
	if pos >= 0 && len(i.index) > pos+1 {
		return i.index[pos+1:]
	}
	return versions
}

func (i *Migrations) Prev(version uint64) (prevVersion uint64, ok bool) {
	pos := i.findPos(version)
	if pos >= 1 && len(i.index) > pos-1 {
		return i.index[pos-1], true
	}
	return 0, false
}

func (i *Migrations) Next(version uint64) (nextVersion uint64, ok bool) {
	pos := i.findPos(version)
	if pos >= 0 && len(i.index) > pos+1 {
		return i.index[pos+1], true
	}
	return 0, false
}

func (i *Migrations) GetDirections(version uint64) map[Direction]bool {
	var directions map[Direction]bool
	directions = map[Direction]bool{
		Down:     false,
		Up:       false,
		MetaDown: false,
		MetaUp:   false,
	}
	for k := range i.migrations[version] {
		directions[k] = true
	}
	return directions
}

func (i *Migrations) Up(version uint64) (m *Migration, ok bool) {
	if _, ok := i.migrations[version]; ok {
		if mx, ok := i.migrations[version][Up]; ok {
			return mx, true
		}
	}
	return nil, false
}

func (i *Migrations) MetaUp(version uint64) (m *Migration, ok bool) {
	if _, ok := i.migrations[version]; ok {
		if mx, ok := i.migrations[version][MetaUp]; ok {
			return mx, true
		}
	}
	return nil, false
}

func (i *Migrations) Down(version uint64) (m *Migration, ok bool) {
	if _, ok := i.migrations[version]; ok {
		if mx, ok := i.migrations[version][Down]; ok {
			return mx, true
		}
	}
	return nil, false
}

func (i *Migrations) MetaDown(version uint64) (m *Migration, ok bool) {
	if _, ok := i.migrations[version]; ok {
		if mx, ok := i.migrations[version][MetaDown]; ok {
			return mx, true
		}
	}
	return nil, false
}

func (i *Migrations) findPos(version uint64) int {
	if len(i.index) > 0 {
		ix := i.index.Search(version)
		if ix < len(i.index) && i.index[ix] == version {
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
