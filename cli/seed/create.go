package seed

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	internalerrors "github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/spf13/afero"
)

// CreateSeedOpts has the list of options required
// to create a seed file
type CreateSeedOpts struct {
	UserProvidedSeedName string
	// DirectoryPath in which seed file should be created
	DirectoryPath string
	Data          io.Reader
	Database      string
}

// CreateSeedFile creates a .sql file according to the arguments
// it'll return full filepath and an error if any
func CreateSeedFile(fs afero.Fs, opts CreateSeedOpts) (*string, error) {
	var op internalerrors.Op = "seed.CreateSeedFile"
	if opts.Data == nil {
		return nil, internalerrors.E(op, errors.New("no data provided"))
	}
	const fileExtension = "sql"

	timestamp := strconv.FormatInt(time.Now().UnixNano()/int64(time.Millisecond), 10)
	// filename will be in format <timestamp>_<userProvidedSeedName>.sql
	filenameWithTimeStamp := fmt.Sprintf("%s_%s.%s", timestamp, opts.UserProvidedSeedName, fileExtension)
	fullFilePath := filepath.Join(filepath.Join(opts.DirectoryPath, opts.Database), filenameWithTimeStamp)

	// Write contents to file
	file, err := fs.OpenFile(fullFilePath, os.O_WRONLY|os.O_CREATE, 0644)
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	defer file.Close()

	r := bufio.NewReader(opts.Data)
	_, err = io.Copy(file, r)
	if err != nil {
		return nil, internalerrors.E(op, err)
	}

	return &fullFilePath, nil
}

func (d *Driver) ExportDatadump(tableNames []string, sourceName string) (io.Reader, error) {
	var op internalerrors.Op = "seed.Driver.ExportDatadump"
	// to support tables starting with capital letters
	modifiedTableNames := make([]string, len(tableNames))

	for idx, val := range tableNames {
		split := strings.Split(val, ".")
		splitLen := len(split)

		if splitLen != 1 && splitLen != 2 {
			return nil, internalerrors.E(op, fmt.Errorf(`invalid schema/table provided "%s"`, val))
		}

		if splitLen == 2 {
			modifiedTableNames[idx] = fmt.Sprintf(`"%s"."%s"`, split[0], split[1])
		} else {
			modifiedTableNames[idx] = fmt.Sprintf(`"%s"`, val)
		}
	}

	pgDumpOpts := []string{"--no-owner", "--no-acl", "--data-only", "--column-inserts"}
	for _, table := range modifiedTableNames {
		pgDumpOpts = append(pgDumpOpts, "--table", table)
	}
	request := hasura.PGDumpRequest{
		Opts:        pgDumpOpts,
		CleanOutput: true,
		SourceName:  sourceName,
	}
	response, err := d.PGDumpClient.Send(request)
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	return response, nil
}
