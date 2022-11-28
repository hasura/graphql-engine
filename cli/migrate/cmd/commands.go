package cmd

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/migrate"
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
	var op errors.Op = "cmd.CreateOptions.SetSQLUpFromFile"
	data, err := ioutil.ReadFile(filePath)
	if err != nil {
		return errors.E(op, err)
	}

	c.SQLUp = data
	return nil
}

func (c *CreateOptions) SetSQLDown(data string) error {
	c.SQLDown = []byte(data)
	return nil
}

func (c *CreateOptions) Create() error {
	var op errors.Op = "cmd.CreateOptions.Create"
	path := filepath.Join(c.Directory, fmt.Sprintf("%s_%s", c.Version, c.Name))
	err := os.MkdirAll(path, os.ModePerm)
	if err != nil {
		return errors.E(op, err)
	}

	// Check if data has been set in one of the files
	if c.SQLUp == nil && c.SQLDown == nil {
		return errors.E(op, "none of the files has been set with data")
	}

	if c.SQLUp != nil {
		// Create SQLUp
		err = createFile(filepath.Join(path, "up.sql"), c.SQLUp)
		if err != nil {
			return errors.E(op, err)
		}
	}

	if c.SQLDown != nil {
		// Create SQLDown
		err = createFile(filepath.Join(path, "down.sql"), c.SQLDown)
		if err != nil {
			return errors.E(op, err)
		}
	}
	return nil
}

func (c *CreateOptions) Delete() error {
	var op errors.Op = "cmd.CreateOptions.Delete"
	files, err := ioutil.ReadDir(c.Directory)
	if err != nil {
		return errors.E(op, err)
	}

	for _, fi := range files {
		if strings.HasPrefix(fi.Name(), fmt.Sprintf("%s_", c.Version)) {
			if fi.IsDir() {
				path := filepath.Join(c.Directory, fi.Name())
				err := deleteFile(path)
				if err != nil {
					return errors.E(op, err)
				}
				return nil
			}
			path := filepath.Join(c.Directory, fi.Name())
			err := deleteFile(path)
			if err != nil {
				return errors.E(op, err)
			}
		}
	}
	return nil
}

func (c *CreateOptions) MoveToDir(destDir string) error {
	var op errors.Op = "cmd.CreateOptions.MoveToDir"
	files, err := ioutil.ReadDir(c.Directory)
	if err != nil {
		return errors.E(op, err)
	}

	for _, fi := range files {
		if strings.HasPrefix(fi.Name(), fmt.Sprintf("%s_", c.Version)) {
			if fi.IsDir() {
				path := filepath.Join(c.Directory, fi.Name())
				err = os.Rename(path, filepath.Join(destDir, fi.Name()))
				if err != nil {
					return errors.E(op, err)
				}
				return nil
			}
		}
	}
	return nil
}

func createFile(fname string, data []byte) error {
	var op errors.Op = "cmd.createFile"
	file, err := os.Create(fname)
	if err != nil {
		return errors.E(op, err)
	}

	defer file.Close()

	_, err = file.Write(data)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func deleteFile(fname string) error {
	var op errors.Op = "cmd.deleteFile"
	err := os.RemoveAll(fname)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func GotoCmd(m *migrate.Migrate, v uint64, direction string) error {
	var op errors.Op = "cmd.GotoCmd"
	err := m.Migrate(v, direction)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func UpCmd(m *migrate.Migrate, limit int64) error {
	var op errors.Op = "cmd.UpCmd"
	var err error
	if limit >= 0 {
		err = m.Steps(limit)
	} else {
		err = m.Up()
	}
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func DownCmd(m *migrate.Migrate, limit int64) error {
	var op errors.Op = "cmd.DownCmd"
	var err error
	if limit >= 0 {
		err = m.Steps(-limit)
	} else {
		err = m.Down()
	}
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func SquashCmd(m *migrate.Migrate, from uint64, to int64, version int64, name, directory string) (versions []int64, err error) {
	var op errors.Op = "cmd.SquashCmd"
	versions, upSql, downSql, err := m.Squash(from, to)
	if err != nil {
		return versions, errors.E(op, err)
	}

	createOptions := New(version, name, directory)
	createOptions.SQLUp = upSql
	createOptions.SQLDown = downSql

	err = createOptions.Create()
	if err != nil {
		return versions, errors.E(op, fmt.Errorf("cannot create migration: %w", err))
	}
	return versions, nil
}

func GotoVersionCmd(m *migrate.Migrate, gotoVersion int64) error {
	var op errors.Op = "cmd.GotoVersionCmd"
	err := m.GotoVersion(gotoVersion)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}
