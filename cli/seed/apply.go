package seed

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/hasura/graphql-engine/cli/v2"

	"github.com/pkg/errors"

	"github.com/spf13/afero"
)

func hasAllowedSeedFileExtensions(filename string) error {
	extension := filepath.Ext(filename)
	allowedExtensions := []string{".sql", ".SQL"}
	for _, allowedExtension := range allowedExtensions {
		if allowedExtension == extension {
			return nil
		}
	}
	return fmt.Errorf("expected extension to be one of %v but got %s on file %s", allowedExtensions, extension, filename)
}

// ApplySeedsToDatabase will read all .sql files in the given
// directory and apply it to hasura
func (d *Driver) ApplySeedsToDatabase(fs afero.Fs, rootSeedsDirectory string, filenames []string, source cli.Source) error {
	seedsDirectory := rootSeedsDirectory
	if len(source.Name) > 0 {
		seedsDirectory = filepath.Join(rootSeedsDirectory, source.Name)
	}
	getSourceKind := func(source cli.Source) hasura.SourceKind {
		if len(source.Name) == 0 {
			return hasura.SourceKindPG
		}
		return source.Kind
	}
	var sqlAsBytes [][]byte
	if len(filenames) > 0 {
		for _, filename := range filenames {
			absFilename := filepath.Join(seedsDirectory, filename)
			if err := hasAllowedSeedFileExtensions(absFilename); err != nil {
				return err
			}
			b, err := afero.ReadFile(fs, absFilename)
			if err != nil {
				return errors.Wrap(err, "error opening file")
			}
			sqlAsBytes = append(sqlAsBytes, b)
		}
	} else {
		err := afero.Walk(fs, seedsDirectory, func(path string, file os.FileInfo, err error) error {
			if file == nil || err != nil {
				return err
			}
			if err := hasAllowedSeedFileExtensions(file.Name()); err == nil && !file.IsDir() {
				b, err := afero.ReadFile(fs, path)
				if err != nil {
					return errors.Wrap(err, "error opening file")
				}
				sqlAsBytes = append(sqlAsBytes, b)
			}
			return nil
		})
		if err != nil {
			return errors.Wrap(err, "error walking the directory path")
		}
	}
	var args []hasura.RequestBody
	sourceKind := getSourceKind(source)
	switch sourceKind {
	case hasura.SourceKindPG:
		for _, sql := range sqlAsBytes {
			request := hasura.RequestBody{
				Type: "run_sql",
				Args: hasura.PGRunSQLInput{
					SQL:    string(sql),
					Source: source.Name,
				},
			}
			args = append(args, request)
		}
	case hasura.SourceKindMSSQL:
		for _, sql := range sqlAsBytes {
			request := hasura.RequestBody{
				Type: "mssql_run_sql",
				Args: hasura.MSSQLRunSQLInput{
					SQL:    string(sql),
					Source: source.Name,
				},
			}
			args = append(args, request)
		}
	case hasura.SourceKindCitus:
		for _, sql := range sqlAsBytes {
			request := hasura.RequestBody{
				Type: "citus_run_sql",
				Args: hasura.CitusRunSQLInput{
					SQL:    string(sql),
					Source: source.Name,
				},
			}
			args = append(args, request)
		}
	default:
		return fmt.Errorf("database %s of kind %s is not supported", source.Name, source.Kind)
	}

	if len(args) == 0 {
		return fmt.Errorf("no SQL files found in %s", seedsDirectory)
	}
	_, err := d.SendBulk(args)
	if err != nil {
		return err
	}
	return nil
}
