package migrate

import (
	"sort"
)

type MigrationStatus struct {
	// Version is the version of this migration.
	Version uint64 `json:"-"`

	// Name helps finding this migration in the source folder
	Name string `json:"-"`

	// Check if the migration is applied on the cluster
	IsApplied bool `json:"database_status"`

	// Check if the migration is present on the local.
	IsPresent bool `json:"source_status"`
}

type Status struct {
	Index      uint64Slice                 `json:"migrations"`
	Migrations map[uint64]*MigrationStatus `json:"status"`
}

func NewStatus() *Status {
	return &Status{
		Index:      make(uint64Slice, 0),
		Migrations: make(map[uint64]*MigrationStatus),
	}
}

func (i *Status) Append(m *MigrationStatus) (ok bool) {
	if m == nil {
		return false
	}

	if i.Migrations[m.Version] == nil {
		i.Migrations[m.Version] = m
	} else {
		// If the Version already exists
		i.Migrations[m.Version].IsApplied = m.IsApplied
	}

	i.buildIndex()
	return true
}

func (i *Status) buildIndex() {
	i.Index = make(uint64Slice, 0)
	for version := range i.Migrations {
		i.Index = append(i.Index, version)
	}
	sort.Sort(i.Index)
}

func (i *Status) Read(version uint64) (m *MigrationStatus, ok bool) {
	if mx, ok := i.Migrations[version]; ok {
		return mx, true
	}
	return nil, false
}

func (i *Status) findPos(version uint64) int {
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
