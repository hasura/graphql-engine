package actions

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"

	gyaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"

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
	ybyt, err := yaml.Marshal(toPayload)
	if err != nil {
		return
	}
	fromByt, err := gyaml.YAMLToJSON(ybyt)
	if err != nil {
		return
	}
	inputFileName, err := writeCLIExtInput(fromByt)
	if err != nil {
		return
	}
	sdlToCmd, err := getCLIExtPath(cmdName)
	if err != nil {
		return
	}
	args := []string{"sdl", "to", "--input-file", inputFileName, "--output-file", outputFileName}
	sdlToCmd.Args = append(sdlToCmd.Args, args...)
	var stdout bytes.Buffer
	var stderr bytes.Buffer
	sdlToCmd.Stdout = &stdout
	sdlToCmd.Stderr = &stderr
	err = sdlToCmd.Run()
	logger.WithField("command", "sdl to").Debug(fmt.Sprintf("output: %s", stdout.String()))
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
	err = yaml.Unmarshal(tmpByt, &toResponse)
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
	sdlFromCmd, err := getCLIExtPath(cmdName)
	if err != nil {
		return
	}
	args := []string{"sdl", "from", "--input-file", inputFileName, "--output-file", outputFileName}
	sdlFromCmd.Args = append(sdlFromCmd.Args, args...)
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
	err = yaml.Unmarshal(tmpByt, &fromResponse)
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
	actionsCodegenCmd, err := getCLIExtPath(cmdName)
	if err != nil {
		return
	}
	args := []string{"actions-codegen", "--input-file", inputFileName, "--output-file", outputFileName}
	actionsCodegenCmd.Args = append(actionsCodegenCmd.Args, args...)
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
	return fmt.Sprintf(`https://raw.githubusercontent.com/%s/master/%s/actions-codegen.js`, util.ActionsCodegenOrg, framework)
}
