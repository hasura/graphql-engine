package cmd

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/migrate"
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
		Directory: filepath.Join(directory, fmt.Sprintf("%s_%s", v, name)),
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
	err := os.MkdirAll(c.Directory, os.ModePerm)
	if err != nil {
		return err
	}

	// Check if data has been set in one of the files
	if c.MetaUp == nil && c.MetaDown == nil && c.SQLUp == nil && c.SQLDown == nil {
		return errors.New("none of the files has been set with data")
	}

	if c.MetaUp != nil {
		// Create MetaUp
		err = createFile(filepath.Join(c.Directory, "up.yaml"), c.MetaUp)
		if err != nil {
			return err
		}
	}

	if c.MetaDown != nil {
		// Create MetaDown
		err = createFile(filepath.Join(c.Directory, "down.yaml"), c.MetaDown)
		if err != nil {
			return err
		}
	}

	if c.SQLUp != nil {
		// Create SQLUp
		err = createFile(filepath.Join(c.Directory, "up.sql"), c.SQLUp)
		if err != nil {
			return err
		}
	}

	if c.SQLDown != nil {
		// Create SQLDown
		err = createFile(filepath.Join(c.Directory, "down.sql"), c.SQLDown)
		if err != nil {
			return err
		}
	}
	return nil
}

func (c *CreateOptions) Delete() error {
	return deleteFile(c.Directory)
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

func ResetCmd(m *migrate.Migrate) error {
	return m.Reset()
}
