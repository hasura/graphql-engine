package actions

import (
	"encoding/json"
	"io/ioutil"
	"os/exec"
)

const (
	tempFile string = "/tmp/logtest.json"
)

func readCliExtOutput() (fileContent string, err error) {

	fileBytes, err := ioutil.ReadFile(tempFile)
	fileContent = string(fileBytes)
	return
}

func convertMetadataToSDL (toPayload sdlToRequest) (toResponse sdlToResponse, err error) {
	fromByt, err := json.Marshal(toPayload)

	if err != nil {
		return
	}

  exec.Command("scaffolder", "sdl", "to", string(fromByt), "--output-file", tempFile)

  response, err := readCliExtOutput()
  if err != nil {
  	return
  }

  err = json.Unmarshal([]byte(response), &toResponse)
  return
}

func convertSDLToMetadata (fromPayload sdlFromRequest) (fromResponse sdlFromResponse, err error) {

	fromByt, err := json.Marshal(fromPayload)

	if err != nil {
		return
	}

  exec.Command("scaffolder", "sdl", "from", string(fromByt), "--output-file", tempFile)

  response, err := readCliExtOutput()
  if err != nil {
  	return
  }

  err = json.Unmarshal([]byte(response), &fromResponse)
  return

}

func getActionsCodegen (codegenReq actionsCodegenRequest) (codegenResp actionsCodegenResponse, err error) {

	fromByt, err := json.Marshal(codegenReq)

	if err != nil {
		return
	}

  exec.Command("scaffolder", "actions-codegen", string(fromByt), "--output-file", tempFile)

  response, err := readCliExtOutput()
  if err != nil {
  	return
  }

  err = json.Unmarshal([]byte(response), &codegenResp)
  return

}