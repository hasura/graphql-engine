package cliextension

import (
	"io/ioutil"
	"os"
)

func writeCLIExtInput(data []byte) (string, error) {
	file, err := ioutil.TempFile("", "*.json")
	if err != nil {
		return "", err
	}
	err = ioutil.WriteFile(file.Name(), data, os.ModePerm)
	if err != nil {
		return "", err
	}
	return file.Name(), nil
}

func readCliExtOutput(filename string) ([]byte, error) {
	fileBytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	return fileBytes, nil
}
