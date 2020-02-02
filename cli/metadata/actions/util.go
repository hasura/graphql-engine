package actions

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"

	"github.com/sirupsen/logrus"

	"github.com/pkg/errors"
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

func convertMetadataToSDL(toPayload sdlToRequest, cmdName string, logger *logrus.Logger) (toResponse sdlToResponse, err error) {
	outputFile, err := ioutil.TempFile("", "*.json")
	if err != nil {
		return
	}
	outputFileName := outputFile.Name()
	// Defer removal of the temporary file in case any of the next steps fail.
	defer os.Remove(outputFileName)
	fromByt, err := json.Marshal(toPayload)
	if err != nil {
		return
	}
	inputFileName, err := writeCLIExtInput(fromByt)
	if err != nil {
		return
	}
	sdlToCmd := exec.Command(cmdName, "cli-ext", "sdl", "to", "--input-file", inputFileName, "--output-file", outputFileName)
	var stdout bytes.Buffer
	var stderr bytes.Buffer
	sdlToCmd.Stdout = &stdout
	sdlToCmd.Stderr = &stderr
	err = sdlToCmd.Run()
	logger.WithField("command", "sdl to").Debugln(fmt.Sprintf("output: %s", stdout.String()))
	if err != nil {
		return toResponse, errors.Wrap(
			fmt.Errorf(stderr.String()),
			err.Error(),
		)
	}
	tmpByt, err := readCliExtOutput(outputFileName)
	if err != nil {
		return
	}
	err = json.Unmarshal(tmpByt, &toResponse)
	return
}

func convertSDLToMetadata(fromPayload sdlFromRequest, cmdName string, logger *logrus.Logger) (fromResponse sdlFromResponse, err error) {
	outputFile, err := ioutil.TempFile("", "*.json")
	if err != nil {
		return
	}
	outputFileName := outputFile.Name()
	// Defer removal of the temporary file in case any of the next steps fail.
	defer os.Remove(outputFileName)
	fromByt, err := json.Marshal(fromPayload)
	if err != nil {
		return
	}
	inputFileName, err := writeCLIExtInput(fromByt)
	if err != nil {
		return
	}
	sdlFromCmd := exec.Command(cmdName, "cli-ext", "sdl", "from", "--input-file", inputFileName, "--output-file", outputFileName)
	var stdout bytes.Buffer
	var stderr bytes.Buffer
	sdlFromCmd.Stdout = &stdout
	sdlFromCmd.Stderr = &stderr
	err = sdlFromCmd.Run()
	logger.WithField("command", "sdl from").Debugln(fmt.Sprintf("output: %s", stdout.String()))
	if err != nil {
		err = errors.Wrap(
			fmt.Errorf(stderr.String()),
			err.Error(),
		)
		return
	}
	tmpByt, err := readCliExtOutput(outputFileName)
	if err != nil {
		return
	}
	err = json.Unmarshal(tmpByt, &fromResponse)
	return
}

func getActionsCodegen(codegenReq actionsCodegenRequest, cmdName string, logger *logrus.Logger) (codegenResp actionsCodegenResponse, err error) {
	outputFile, err := ioutil.TempFile("", "*.json")
	if err != nil {
		return
	}
	outputFileName := outputFile.Name()
	// Defer removal of the temporary file in case any of the next steps fail.
	defer os.Remove(outputFileName)
	fromByt, err := json.Marshal(codegenReq)
	if err != nil {
		return
	}
	inputFileName, err := writeCLIExtInput(fromByt)
	if err != nil {
		return
	}
	actionsCodegenCmd := exec.Command(cmdName, "cli-ext", "actions-codegen", "--input-file", inputFileName, "--output-file", outputFileName)
	var stdout bytes.Buffer
	var stderr bytes.Buffer
	actionsCodegenCmd.Stdout = &stdout
	actionsCodegenCmd.Stderr = &stderr
	err = actionsCodegenCmd.Run()
	logger.WithField("command", "actions-codegen").Debugln(fmt.Sprintf("output: %s", stdout.String()))
	if err != nil {
		err = errors.Wrap(
			fmt.Errorf(stderr.String()),
			err.Error(),
		)
		return
	}
	tmpByt, err := readCliExtOutput(outputFileName)
	if err != nil {
		return
	}
	err = json.Unmarshal(tmpByt, &codegenResp)
	if err != nil {
		return
	}
	return
}

func getActionsCodegenURI(framework string) string {
	return fmt.Sprintf(`https://raw.githubusercontent.com/%s/master/%s/codegen.js`, actionsCodegenRepo, framework)
}
