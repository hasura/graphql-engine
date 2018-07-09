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

func DeleteCmd(dir string, timestamp int64) error {
	count := 0
	if runtime.GOOS == "windows" {
		dir = strings.TrimPrefix(dir, "/")
	}
	fileName := fmt.Sprintf("%v_", timestamp)
	// scan directory
	files, err := ioutil.ReadDir(dir)
	if err != nil {
		return err
	}

	for _, fi := range files {
		if !fi.IsDir() {
			if strings.HasPrefix(fi.Name(), fileName) {
				base := filepath.Join(dir, fi.Name())
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

func CreateCmd(dir string, timestamp int64, name string, options ...interface{}) error {
	if runtime.GOOS == "windows" {
		dir = strings.TrimPrefix(dir, "/")
	}
	fileName := fmt.Sprintf("%v_%v.", timestamp, name)
	base := filepath.Join(dir, fileName)
	err := os.MkdirAll(dir, os.ModePerm)
	if err != nil {
		return err
	}

	// If len(options) == 0, cmd else, api
	if len(options) == 0 {
		return createForCMD(base)
	}
	return createForAPI(base, options[0])
}

func createForCMD(base string) error {
	var data []byte
	var err error
	for _, v := range ext {
		switch v {
		case sqlFile:
			data = []byte{}
		case yamlFile:
			bytes := []byte(`[]`)
			data, err = yaml.JSONToYAML(bytes)
			if err != nil {
				return err
			}
		}
		err = createFile(base+"up"+v, data)
		if err != nil {
			return err
		}
		err = createFile(base+"down"+v, data)
		if err != nil {
			return err
		}
	}
	return nil
}

func createForAPI(base string, options interface{}) error {
	var data []byte
	for _, v := range ext {
		switch v {
		// Only yaml file for api-console
		case yamlFile:
			// Up file
			t, err := json.Marshal(options)
			if err != nil {
				return err
			}

			data, err = yaml.JSONToYAML(t)
			if err != nil {
				return err
			}

			err = createFile(base+"up"+v, data)
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
