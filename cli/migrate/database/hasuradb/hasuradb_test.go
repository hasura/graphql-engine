package hasuradb

import (
	"database/sql"
	sqldriver "database/sql/driver"
	"fmt"
	"io"

	mt "github.com/hasura/graphql-engine/cli/migrate/testing"
	_ "github.com/lib/pq"
	"github.com/parnurzeal/gorequest"
)

var postgresVersions = []mt.Version{
	{Image: "postgres:9.6"},
}

var ravenVersions = []mt.Version{
	{Image: "hasura/graphql-engine:190d78e", Cmd: []string{"raven", "serve", "--database-url"}, ExposedPort: 8080},
}

func isReadyPostgres(i mt.Instance) bool {
	db, err := sql.Open("postgres", fmt.Sprintf("postgres://postgres@%v:%v/postgres?sslmode=disable", i.Host(), i.Port()))
	if err != nil {
		return false
	}

	defer db.Close()
	if err = db.Ping(); err != nil {
		switch err {
		case sqldriver.ErrBadConn, io.EOF:
			return false
		default:
			fmt.Println(err)
		}
		return false
	}
	return true
}

func isReadyRaven(i mt.Instance) bool {
	request := gorequest.New()
	_, _, errs := request.Post(fmt.Sprintf("http://%s:%d", i.Host(), i.Port())).End()
	if len(errs) == 0 {
		return true
	}
	return false
}

/*
func testHasuraDB(t *testing.T) {
	mt.ParallelTest(t, postgresVersions, isReadyPostgres,
		func(t *testing.T, pi mt.Instance) {
			for i, v := range ravenVersions {
				ravenVersions[i].Cmd = append(v.Cmd, fmt.Sprintf("postgres://postgres@%v:%v/postgres?sslmode=disable", pi.NetworkSettings().Gateway, pi.Port()))
			}
			mt.ParallelTest(t, ravenVersions, isReadyRaven,
				func(t *testing.T, ri mt.Instance) {
					defer pi.Remove()
					defer ri.Remove()

					h := &HasuraDB{}
					addr := fmt.Sprintf("hasuradb://@%v:%v", ri.Host(), ri.Port())
					d, err := h.Open(addr, true)
					if err != nil {
						t.Fatalf("%v", err)
					}
					defer d.Close()
				})
		})
}
*/
