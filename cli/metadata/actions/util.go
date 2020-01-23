package actions

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"

	"github.com/pkg/errors"
)

func readCliExtOutput(filename string) ([]byte, error) {
	fileBytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	return fileBytes, nil
}

func convertMetadataToSDL(toPayload sdlToRequest, cmdName string) (toResponse sdlToResponse, err error) {
	file, err := ioutil.TempFile("", "*.json")
	if err != nil {
		return
	}
	filename := file.Name()
	// Defer removal of the temporary file in case any of the next steps fail.
	defer os.Remove(filename)
	fromByt, err := json.Marshal(toPayload)
	if err != nil {
		return
	}
	sdlToCmd := exec.Command(cmdName, "cli-ext", "sdl", "to", string(fromByt), "--output-file", filename)
	var stderr bytes.Buffer
	sdlToCmd.Stderr = &stderr
	err = sdlToCmd.Run()
	if err != nil {
		err = errors.Wrap(
			fmt.Errorf(stderr.String()),
			err.Error(),
		)
		return
	}
	if err != nil {
		return
	}
	tmpByt, err := readCliExtOutput(filename)
	if err != nil {
		return
	}
	err = json.Unmarshal(tmpByt, &toResponse)
	return
}

func convertSDLToMetadata(fromPayload sdlFromRequest, cmdName string) (fromResponse sdlFromResponse, err error) {
	file, err := ioutil.TempFile("", "*.json")
	if err != nil {
		return
	}
	filename := file.Name()
	// Defer removal of the temporary file in case any of the next steps fail.
	defer os.Remove(filename)
	fromByt, err := json.Marshal(fromPayload)
	if err != nil {
		return
	}
	sdlFromCmd := exec.Command(cmdName, "cli-ext", "sdl", "from", string(fromByt), "--output-file", filename)
	var stderr bytes.Buffer
	sdlFromCmd.Stderr = &stderr
	err = sdlFromCmd.Run()
	if err != nil {
		err = errors.Wrap(
			fmt.Errorf(stderr.String()),
			err.Error(),
		)
		return
	}
	if err != nil {
		return
	}
	tmpByt, err := readCliExtOutput(filename)
	if err != nil {
		return
	}
	err = json.Unmarshal(tmpByt, &fromResponse)
	return
}

func getActionsCodegen(codegenReq actionsCodegenRequest, cmdName string) (codegenResp actionsCodegenResponse, err error) {
	file, err := ioutil.TempFile("", "*.json")
	if err != nil {
		return
	}
	filename := file.Name()
	// Defer removal of the temporary file in case any of the next steps fail.
	defer os.Remove(filename)
	fromByt, err := json.Marshal(codegenReq)
	if err != nil {
		return
	}
	actionsCodegenCmd := exec.Command(cmdName, "cli-ext", "actions-codegen", string(fromByt), "--output-file", filename)
	var stderr bytes.Buffer
	actionsCodegenCmd.Stderr = &stderr
	err = actionsCodegenCmd.Run()
	if err != nil {
		err = errors.Wrap(
			fmt.Errorf(stderr.String()),
			err.Error(),
		)
		return
	}
	tmpByt, err := readCliExtOutput(filename)
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
