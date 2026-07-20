package database

import (
	"container/list"
	"iter"
)

type CustomList struct {
	List *list.List
}

func (c *CustomList) Iterate() iter.Seq[any] {
	return func(yield func(any) bool) {
		for e := c.List.Front(); e != nil; e = e.Next() {
			if !yield(e.Value) {
				return
			}
		}
	}
}

type Database struct {
	Name string
	Type string
}
