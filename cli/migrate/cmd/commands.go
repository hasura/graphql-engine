package cmd

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"runtime"
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
	Version   int64
	Directory string
	Name      string
	MetaUp    []byte
	MetaDown  []byte
	SQLUp     []byte
	SQLDown   []byte
}

func New(version int64, name, directory string) *CreateOptions {
	if runtime.GOOS == "windows" {
		directory = strings.TrimPrefix(directory, "/")
	}
	return &CreateOptions{
		Version:   version,
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
	fileName := fmt.Sprintf("%v_%v.", c.Version, c.Name)
	base := filepath.Join(c.Directory, fileName)
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
		err = createFile(base+"up.yaml", c.MetaUp)
		if err != nil {
			return err
		}
	}

	if c.MetaDown != nil {
		// Create MetaDown
		err = createFile(base+"down.yaml", c.MetaDown)
		if err != nil {
			return err
		}
	}

	if c.SQLUp != nil {
		// Create SQLUp
		err = createFile(base+"up.sql", c.SQLUp)
		if err != nil {
			return err
		}
	}

	if c.SQLDown != nil {
		// Create SQLDown
		err = createFile(base+"down.sql", c.SQLDown)
		if err != nil {
			return err
		}
	}
	return nil
}

func (c *CreateOptions) Delete() error {
	count := 0
	fileName := fmt.Sprintf("%v_", c.Version)
	// scan directory
	files, err := ioutil.ReadDir(c.Directory)
	if err != nil {
		return err
	}

	for _, fi := range files {
		if !fi.IsDir() {
			if strings.HasPrefix(fi.Name(), fileName) {
				base := filepath.Join(c.Directory, fi.Name())
				err = deleteFile(base)
				if err != nil {
					return err
				}
				count = count + 1
			}
		}
	}
	if count == 0 {
		return errors.New("Cannot find any migration file")
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
	err := os.RemoveAll(fname)
	return err
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
