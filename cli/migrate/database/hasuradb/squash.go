package hasuradb

import (
	"io"
	"io/ioutil"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/migrate/database"
)

func (h *HasuraDB) PushToList(migration io.Reader, fileType string, l *database.CustomList) error {
	var op errors.Op = "hasuradb.HasuraDB.PushToList"
	migr, err := ioutil.ReadAll(migration)
	if err != nil {
		return errors.E(op, err)
	}
	body := string(migr[:])
	switch fileType {
	case "sql":
		if body == "" {
			break
		}
		tt := &hasura.PGRunSQLInput{
			SQL: body,
		}
		l.List.PushBack(tt)
	default:
		return errors.E(op, "invalid migration file type")
	}
	return nil
}

func (h *HasuraDB) Squash(l *database.CustomList, ret chan<- interface{}) {
	var op errors.Op = "hasuradb.HasuraDB.Squash"
	for e := l.List.Front(); e != nil; e = e.Next() {
		switch args := e.Value.(type) {
		case *hasura.PGRunSQLInput:
			ret <- []byte(args.SQL)
			continue
		default:
			h.logger.Debug("cannot find metadata type for:", args)
			ret <- errors.E(op, "invalid metadata action")
			return
		}
	}
}
