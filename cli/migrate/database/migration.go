package database

import "sort"

// Migrations wraps Migration and has an internal index
// to keep track of Migration order in database.
type Migrations struct {
	index uint64Slice
}

func NewMigrations() *Migrations {
	return &Migrations{
		index: make(uint64Slice, 0),
	}
}

func (i *Migrations) Append(version uint64) {
	if i.findPos(version) > 0 {
		return
	}
	i.index = append(i.index, version)
	sort.Sort(i.index)
}

func (i *Migrations) First() (version uint64, ok bool) {
	if len(i.index) == 0 {
		return 0, false
	}
	return i.index[0], true
}

func (i *Migrations) Last() (uint64, bool) {
	if len(i.index) == 0 {
		return 0, false
	}

	return i.index[len(i.index)-1], true
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

func (i *Migrations) Read(version uint64) (ok bool) {
	pos := i.findPos(version)
	if pos >= 0 {
		return true
	}
	return false
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
