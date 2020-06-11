package seed

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"time"

	"github.com/spf13/afero"
)

// CreateSeedOpts has the list of options required
// to create a seed file
type CreateSeedOpts struct {
	UserProvidedSeedName string
	// DirectoryPath in which seed file should be created
	DirectoryPath string
	Data          io.Reader
}

// CreateSeedFile creates a .sql file according to the arguments
// it'll return full filepath and an error if any
func CreateSeedFile(fs afero.Fs, opts CreateSeedOpts) (*string, error) {
	if opts.Data == nil {
		return nil, errors.New("no data provided")
	}
	const fileExtension = "sql"

	timestamp := strconv.FormatInt(time.Now().UnixNano()/int64(time.Millisecond), 10)
	// filename will be in format <timestamp>_<userProvidedSeedName>.sql
	filenameWithTimeStamp := fmt.Sprintf("%s_%s.%s", timestamp, opts.UserProvidedSeedName, fileExtension)
	fullFilePath := filepath.Join(opts.DirectoryPath, filenameWithTimeStamp)

	// Write contents to file
	file, err := fs.OpenFile(fullFilePath, os.O_WRONLY|os.O_CREATE, 0644)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	r := bufio.NewReader(opts.Data)
	io.Copy(file, r)

	return &fullFilePath, nil
}
