package cliextension

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"

	gyaml "github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions/types"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v3"
)

// Config represents the object to interact with cli-ext
type Config struct {
	binPath *string
	logger  *logrus.Logger
}

// NewCLIExtensionConfig creates CLIExtensionConfig to interact with cli-extension
func NewCLIExtensionConfig(binPath *string, logger *logrus.Logger) *Config {
	return &Config{
		binPath: binPath,
		logger:  logger,
	}
}

// ConvertMetadataToSDL converts actions metadata to graphql SDL
func (c *Config) ConvertMetadataToSDL(toPayload types.SDLToRequest) (types.SDLToResponse, error) {
	var op errors.Op = "cliextension.Config.ConvertMetadataToSDL"
	var toResponse types.SDLToResponse
	outputFile, err := ioutil.TempFile("", "*.json")
	if err != nil {
		return toResponse, errors.E(op, err)
	}
	outputFileName := outputFile.Name()
	// Defer removal of the temporary file in case any of the next steps fail.
	defer os.Remove(outputFileName)
	ybyt, err := yaml.Marshal(toPayload)
	if err != nil {
		return toResponse, errors.E(op, err)
	}
	fromByt, err := gyaml.YAMLToJSON(ybyt)
	if err != nil {
		return toResponse, errors.E(op, err)
	}
	inputFileName, err := writeCLIExtInput(fromByt)
	if err != nil {
		return toResponse, errors.E(op, err)
	}
	sdlToCmd := exec.Command(*c.binPath)
	args := []string{"sdl", "to", "--input-file", inputFileName, "--output-file", outputFileName}
	sdlToCmd.Args = append(sdlToCmd.Args, args...)
	var stdout bytes.Buffer
	var stderr bytes.Buffer
	sdlToCmd.Stdout = &stdout
	sdlToCmd.Stderr = &stderr
	err = sdlToCmd.Run()
	c.logger.WithField("command", "sdl to").Debug(fmt.Sprintf("output: %s", stdout.String()))
	if err != nil {
		return toResponse, errors.E(op, fmt.Errorf("%s: %s", err.Error(), stderr.String()))
	}
	tmpByt, err := readCliExtOutput(outputFileName)
	if err != nil {
		return toResponse, errors.E(op, err)
	}
	err = yaml.Unmarshal(tmpByt, &toResponse)
	if err != nil {
		return toResponse, errors.E(op, errors.KindBadInput, err)
	}
	return toResponse, nil
}

// ConvertSDLToMetadata converts graphql SDL to hasura metadata
func (c *Config) ConvertSDLToMetadata(fromPayload types.SDLFromRequest) (types.SDLFromResponse, error) {
	var op errors.Op = "cliextension.Config.ConvertSDLToMetadata"
	var fromResponse types.SDLFromResponse
	outputFile, err := ioutil.TempFile("", "*.json")
	if err != nil {
		return fromResponse, errors.E(op, err)
	}
	outputFileName := outputFile.Name()
	// Defer removal of the temporary file in case any of the next steps fail.
	defer os.Remove(outputFileName)
	fromByt, err := json.Marshal(fromPayload)
	if err != nil {
		return fromResponse, errors.E(op, err)
	}
	inputFileName, err := writeCLIExtInput(fromByt)
	if err != nil {
		return fromResponse, errors.E(op, err)
	}
	sdlFromCmd := exec.Command(*c.binPath)
	args := []string{"sdl", "from", "--input-file", inputFileName, "--output-file", outputFileName}
	sdlFromCmd.Args = append(sdlFromCmd.Args, args...)
	var stdout bytes.Buffer
	var stderr bytes.Buffer
	sdlFromCmd.Stdout = &stdout
	sdlFromCmd.Stderr = &stderr
	err = sdlFromCmd.Run()
	c.logger.WithField("command", "sdl from").Debugln(fmt.Sprintf("output: %s", stdout.String()))
	if err != nil {
		return fromResponse, errors.E(op, fmt.Errorf("%s: %s", err.Error(), stderr.String()))
	}
	tmpByt, err := readCliExtOutput(outputFileName)
	if err != nil {
		return fromResponse, errors.E(op, err)
	}
	err = yaml.Unmarshal(tmpByt, &fromResponse)
	if err != nil {
		return fromResponse, errors.E(op, errors.KindBadInput, err)
	}
	return fromResponse, nil
}

// GetActionsCodegen generates codegen for an action
func (c *Config) GetActionsCodegen(codegenReq types.ActionsCodegenRequest) (types.ActionsCodegenResponse, error) {
	var op errors.Op = "cliextension.Config.GetActionsCodegen"
	var codegenResp types.ActionsCodegenResponse
	outputFile, err := ioutil.TempFile("", "*.json")
	if err != nil {
		return codegenResp, errors.E(op, err)
	}
	outputFileName := outputFile.Name()
	// Defer removal of the temporary file in case any of the next steps fail.
	defer os.Remove(outputFileName)
	fromByt, err := json.Marshal(codegenReq)
	if err != nil {
		return codegenResp, errors.E(op, err)
	}
	inputFileName, err := writeCLIExtInput(fromByt)
	if err != nil {
		return codegenResp, errors.E(op, err)
	}
	actionsCodegenCmd := exec.Command(*c.binPath)
	args := []string{"actions-codegen", "--input-file", inputFileName, "--output-file", outputFileName}
	actionsCodegenCmd.Args = append(actionsCodegenCmd.Args, args...)
	var stdout bytes.Buffer
	var stderr bytes.Buffer
	actionsCodegenCmd.Stdout = &stdout
	actionsCodegenCmd.Stderr = &stderr
	err = actionsCodegenCmd.Run()
	c.logger.WithField("command", "actions-codegen").Debugln(fmt.Sprintf("output: %s", stdout.String()))
	if err != nil {
		return codegenResp, errors.E(op, fmt.Errorf("%s: %s", err.Error(), stderr.String()))
	}
	tmpByt, err := readCliExtOutput(outputFileName)
	if err != nil {
		return codegenResp, errors.E(op, err)
	}
	err = json.Unmarshal(tmpByt, &codegenResp)
	if err != nil {
		return codegenResp, errors.E(op, errors.KindBadInput, err)
	}
	return codegenResp, nil
}
