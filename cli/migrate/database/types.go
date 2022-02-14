package database

import (
	"container/list"

	"github.com/ahmetb/go-linq"
)

type CustomList struct {
	List *list.List
}

func (c *CustomList) Iterate() linq.Iterator {
	length := c.List.Len()
	var prevElem *list.Element
	i := 0
	return func() (item interface{}, ok bool) {
		if length == 0 {
			return
		}

		if i == 0 {
			prevElem = c.List.Front()
			i++
		} else {
			prevElem = prevElem.Next()
			if prevElem == nil {
				return
			}
		}
		return prevElem, true
	}
}

type Database struct {
	Name string
	Type string
}
