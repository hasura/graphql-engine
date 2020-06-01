package seed

import (
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/migrate"

	"github.com/pkg/errors"

	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb"
	"github.com/spf13/afero"
)

// ApplySeedsToDatabase will read all .sql files in the given
// directory and apply it to hasura
func ApplySeedsToDatabase(fs afero.Fs, m *migrate.Migrate, directoryPath string, fileName string) error {
	seedQuery := hasuradb.HasuraInterfaceBulk{
		Type: "bulk",
		Args: make([]interface{}, 0),
	}

	if fileName != "" {
		absFileName, err := filepath.Abs(fileName)
		if err != nil {
			return errors.Wrap(err, "error getting abolute filepath")
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
		err := afero.Walk(fs, directoryPath, func(path string, file os.FileInfo, err error) error {
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
	return m.ApplySeed(seedQuery)
}
