package seed

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli"

	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb"
	"github.com/pkg/errors"

	"github.com/spf13/afero"
)

// ApplySeedsToDatabase will read all .sql files in the given
// directory and apply it to hasura
func ApplySeedsToDatabase(ec *cli.ExecutionContext, fs afero.Fs, m *migrate.Migrate, fileName string) error {
	seedQuery := hasuradb.HasuraInterfaceBulk{
		Type: "bulk",
		Args: make([]interface{}, 0),
	}

	if fileName != "" {
		absFileName, err := filepath.Abs(fileName)
		if err != nil {
			return errors.Wrap(err, "error getting absolute filepath")
		}
		b, err := afero.ReadFile(fs, absFileName)
		if err != nil {
			return errors.Wrap(err, "error opening file")
		}
		q := hasuradb.HasuraInterfaceQuery{
			Type: "run_sql",
			Args: hasuradb.HasuraArgs{
				SQL: string(b),
			},
		}
		seedQuery.Args = append(seedQuery.Args, q)
	} else {
		err := afero.Walk(fs, ec.SeedsDirectory, func(path string, file os.FileInfo, err error) error {
			if !file.IsDir() && filepath.Ext(file.Name()) == ".sql" {
				b, err := afero.ReadFile(fs, path)
				if err != nil {
					return errors.Wrap(err, "error opening file")
				}
				q := hasuradb.HasuraInterfaceQuery{
					Type: "run_sql",
					Args: hasuradb.HasuraArgs{
						SQL: string(b),
					},
				}
				seedQuery.Args = append(seedQuery.Args, q)
			}

			return nil
		})
		if err != nil {
			return errors.Wrap(err, "error walking the directory path")
		}
	}
	if len(seedQuery.Args) == 0 {
		return fmt.Errorf("no SQL files found in %s", ec.SeedsDirectory)
	}
	return m.ApplySeed(seedQuery)
}
