package util

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"path/filepath"
	"strconv"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/migrate"
	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
	"github.com/pkg/errors"
)

const (
	INVALID_STEP = "Cannot parse the number of steps. Should be a positive integer"
)

func GetValidStepFromString(stepString string) (step int64, err error) {
	step, err = strconv.ParseInt(stepString, 10, 64)
	if err != nil {
		return 0, errors.Wrap(err, "not a valid input")
	}
	return step, nil
}

func ExecuteMigration(cmd, dir, db string, stepOrVersion int64) error {
	var err error

	t, err := migrate.New(dir, db, true)
	if err != nil {
		return errors.Wrap(err, "cannot create migrate instance")
	}

	switch cmd {
	case "up":
		err = mig.UpCmd(t, stepOrVersion)
	case "down":
		err = mig.DownCmd(t, stepOrVersion)
	case "version":
		var direction string
		if stepOrVersion >= 0 {
			direction = "up"
		} else {
			direction = "down"
			stepOrVersion = -(stepOrVersion)
		}
		err = mig.GotoCmd(t, uint64(stepOrVersion), direction)
	default:
		err = fmt.Errorf("Invalid command")
	}

	return err
}

func ExecuteMetadata(cmd, dir, db, metadata string) error {
	var err error

	t, err := migrate.New(dir, db, true)
	if err != nil {
		return errors.Wrap(err, "cannot create migrate instance")
	}

	switch cmd {
	case "export":
		metaData, err := t.ExportMetadata()
		if err != nil {
			return errors.Wrap(err, "Cannot export metadata")
		}

		t, err := json.Marshal(metaData)
		if err != nil {
			return errors.Wrap(err, "Cannot Marshal metadata")
		}

		data, err := yaml.JSONToYAML(t)
		if err != nil {
			return err
		}

		err = ioutil.WriteFile(filepath.Join(metadata, "metadata.yaml"), data, 0644)
		if err != nil {
			return errors.Wrap(err, "cannot save metadata")
		}
	case "reset":
		err := t.ResetMetadata()
		if err != nil {
			return errors.Wrap(err, "Cannot reset Metadata")
		}
	case "apply":
		data, err := ioutil.ReadFile(filepath.Join(metadata, "metadata.yaml"))
		if err != nil {
			return errors.Wrap(err, "cannot read metadata file")
		}

		var q interface{}
		err = yaml.Unmarshal(data, &q)
		if err != nil {
			return errors.Wrap(err, "cannot parse metadata file")
		}

		err = t.ApplyMetadata(q)
		if err != nil {
			return errors.Wrap(err, "cannot apply metadata on the database")
		}
	}
	return nil
}

func ExecuteStatus(dir, db string) (*migrate.Status, error) {
	var err error

	t, err := migrate.New(dir, db, true)
	if err != nil {
		return nil, errors.Wrap(err, "cannot create migrate instance")
	}

	status, err := t.GetStatus()
	if err != nil {
		return nil, err
	}
	return status, nil
}
