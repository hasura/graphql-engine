package cmd

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/pkg/errors"
)

const (
	sqlFile  = ".sql"
	yamlFile = ".yaml"
)

var ext = []string{sqlFile, yamlFile}

type CreateOptions struct {
	Version   string
	Directory string
	Name      string
	MetaUp    []byte
	MetaDown  []byte
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

func (c *CreateOptions) SetMetaUp(data interface{}) error {
	t, err := json.Marshal(data)
	if err != nil {
		return err
	}
	yamlData, err := yaml.JSONToYAML(t)
	if err != nil {
		return err
	}
	c.MetaUp = yamlData
	return nil
}

func (c *CreateOptions) SetMetaUpFromFile(filePath string) error {
	data, err := ioutil.ReadFile(filePath)
	if err != nil {
		return err
	}

	var metadata []interface{}
	var q interface{}
	err = yaml.Unmarshal(data, &q)
	if err != nil {
		return err
	}

	metadata = append(
		metadata,
		map[string]interface{}{
			"type": "replace_metadata",
			"args": q,
		},
	)
	return c.SetMetaUp(metadata)
}

func (c *CreateOptions) SetMetaDown(data interface{}) error {
	t, err := json.Marshal(data)
	if err != nil {
		return err
	}
	yamlData, err := yaml.JSONToYAML(t)
	if err != nil {
		return err
	}
	c.MetaDown = yamlData
	return nil
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
	if c.MetaUp == nil && c.MetaDown == nil && c.SQLUp == nil && c.SQLDown == nil {
		return errors.New("none of the files has been set with data")
	}

	if c.MetaUp != nil {
		// Create MetaUp
		err = createFile(filepath.Join(path, "up.yaml"), c.MetaUp)
		if err != nil {
			return err
		}
	}

	if c.MetaDown != nil {
		// Create MetaDown
		err = createFile(filepath.Join(path, "down.yaml"), c.MetaDown)
		if err != nil {
			return err
		}
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

func SquashCmd(m *migrate.Migrate, from uint64, version int64, name, directory string) (versions []int64, err error) {
	versions, upMeta, upSql, downMeta, downSql, err := m.Squash(from)
	if err != nil {
		return
	}

	createOptions := New(version, name, directory)
	if len(upMeta) != 0 {
		byteUp, err := yaml.Marshal(upMeta)
		if err != nil {
			return versions, errors.Wrap(err, "cannot unmarshall up query")
		}
		createOptions.MetaUp = byteUp
	}
	if len(downMeta) != 0 {
		byteDown, err := yaml.Marshal(downMeta)
		if err != nil {
			return versions, errors.Wrap(err, "cannot unmarshall down query")
		}
		createOptions.MetaDown = byteDown
	}
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
