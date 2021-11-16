package cmd

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/migrate"
	"github.com/pkg/errors"
)

/*
const (
	sqlFile  = ".sql"
	yamlFile = ".yaml"
)

var ext = []string{sqlFile, yamlFile}
*/

type CreateOptions struct {
	Version   string
	Directory string
	Name      string
	SQLUp     []byte
	SQLDown   []byte
}

func New(version int64, name, directory string) *CreateOptions {
	v := strconv.FormatInt(version, 10)
	if runtime.GOOS == "windows" {
		directory = strings.TrimPrefix(directory, "/")
	}
	return &CreateOptions{
		Version:   v,
		Directory: directory,
		Name:      name,
	}
}

func (c *CreateOptions) SetSQLUp(data string) error {
	c.SQLUp = []byte(data)
	return nil
}

func (c *CreateOptions) SetSQLUpFromFile(filePath string) error {
	data, err := ioutil.ReadFile(filePath)
	if err != nil {
		return err
	}

	c.SQLUp = data
	return nil
}

func (c *CreateOptions) SetSQLDown(data string) error {
	c.SQLDown = []byte(data)
	return nil
}

func (c *CreateOptions) Create() error {
	path := filepath.Join(c.Directory, fmt.Sprintf("%s_%s", c.Version, c.Name))
	err := os.MkdirAll(path, os.ModePerm)
	if err != nil {
		return err
	}

	// Check if data has been set in one of the files
	if c.SQLUp == nil && c.SQLDown == nil {
		return errors.New("none of the files has been set with data")
	}

	if c.SQLUp != nil {
		// Create SQLUp
		err = createFile(filepath.Join(path, "up.sql"), c.SQLUp)
		if err != nil {
			return err
		}
	}

	if c.SQLDown != nil {
		// Create SQLDown
		err = createFile(filepath.Join(path, "down.sql"), c.SQLDown)
		if err != nil {
			return err
		}
	}
	return nil
}

func (c *CreateOptions) Delete() error {
	files, err := ioutil.ReadDir(c.Directory)
	if err != nil {
		return err
	}

	for _, fi := range files {
		if strings.HasPrefix(fi.Name(), fmt.Sprintf("%s_", c.Version)) {
			if fi.IsDir() {
				path := filepath.Join(c.Directory, fi.Name())
				return deleteFile(path)
			}
			path := filepath.Join(c.Directory, fi.Name())
			err := deleteFile(path)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func (c *CreateOptions) MoveToDir(destDir string) error {
	files, err := ioutil.ReadDir(c.Directory)
	if err != nil {
		return err
	}

	for _, fi := range files {
		if strings.HasPrefix(fi.Name(), fmt.Sprintf("%s_", c.Version)) {
			if fi.IsDir() {
				path := filepath.Join(c.Directory, fi.Name())
				return os.Rename(path, filepath.Join(destDir, fi.Name()))
			}
		}
	}
	return nil
}

func createFile(fname string, data []byte) error {
	file, err := os.Create(fname)
	if err != nil {
		return err
	}

	defer file.Close()

	_, err = file.Write(data)
	if err != nil {
		return err
	}
	return nil
}

func deleteFile(fname string) error {
	return os.RemoveAll(fname)
}

func GotoCmd(m *migrate.Migrate, v uint64, direction string) error {
	return m.Migrate(v, direction)
}

func UpCmd(m *migrate.Migrate, limit int64) error {
	if limit >= 0 {
		return m.Steps(limit)
	} else {
		return m.Up()
	}
}

func DownCmd(m *migrate.Migrate, limit int64) error {
	if limit >= 0 {
		return m.Steps(-limit)
	} else {
		return m.Down()
	}
}

func SquashCmd(m *migrate.Migrate, from uint64, to int64, version int64, name, directory string) (versions []int64, err error) {
	versions, upSql, downSql, err := m.Squash(from, to)
	if err != nil {
		return
	}

	createOptions := New(version, name, directory)
	createOptions.SQLUp = upSql
	createOptions.SQLDown = downSql

	err = createOptions.Create()
	if err != nil {
		return versions, errors.Wrap(err, "cannot create migration")
	}

	return
}

func GotoVersionCmd(m *migrate.Migrate, gotoVersion int64) error {
	return m.GotoVersion(gotoVersion)
}
