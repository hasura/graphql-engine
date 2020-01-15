package actions

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
)

func readCliExtOutput(filename string) ([]byte, error) {
	fileBytes, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Println("hererererere")
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
	_, err = exec.Command(cmdName, "cli-ext", "sdl", "to", string(fromByt), "--output-file", filename).Output()
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
	_, err = exec.Command(cmdName, "cli-ext", "sdl", "from", string(fromByt), "--output-file", filename).CombinedOutput()
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
		fmt.Println(stderr.String())
		return
	}
	tmpByt, err := readCliExtOutput(filename)
	if err != nil {
		return
	}
	err = json.Unmarshal(tmpByt, &codegenResp)
	fmt.Println(string(tmpByt))
	if err != nil {
		return
	}
	return
}

