package cliextension

import (
	"io/ioutil"
	"os"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

func writeCLIExtInput(data []byte) (string, error) {
	var op errors.Op = "cliextension.writeCLIExtInput"
	file, err := ioutil.TempFile("", "*.json")
	if err != nil {
		return "", errors.E(op, err)
	}
	err = ioutil.WriteFile(file.Name(), data, os.ModePerm)
	if err != nil {
		return "", errors.E(op, err)
	}
	return file.Name(), nil
}

func readCliExtOutput(filename string) ([]byte, error) {
	var op errors.Op = "cliextension.readCliExtOutput"
	fileBytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return fileBytes, nil
}
