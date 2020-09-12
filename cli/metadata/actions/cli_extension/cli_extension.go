package cliextension

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli"

	gyaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/metadata/actions/types"
	"github.com/hasura/graphql-engine/cli/plugins"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"
)

// Config represents the object to interact with cli-ext plugin
type Config struct {
	binPath string
	logger  *logrus.Logger
}

// NewCLIExtensionConfig creates CLIExtensionConfig to interact with cli-extension plugin
func NewCLIExtensionConfig(binDir string, logger *logrus.Logger) *Config {
	return &Config{
		binPath: filepath.Join(binDir, plugins.PluginNameToBin(cli.CLIExtPluginName, plugins.IsWindows())),
		logger:  logger,
	}
}

// ConvertMetadataToSDL converts actions metadata to graphql SDL
func (c *Config) ConvertMetadataToSDL(toPayload types.SDLToRequest) (toResponse types.SDLToResponse, err error) {
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
	sdlToCmd := exec.Command(c.binPath)
	args := []string{"sdl", "to", "--input-file", inputFileName, "--output-file", outputFileName}
	sdlToCmd.Args = append(sdlToCmd.Args, args...)
	var stdout bytes.Buffer
	var stderr bytes.Buffer
	sdlToCmd.Stdout = &stdout
	sdlToCmd.Stderr = &stderr
	err = sdlToCmd.Run()
	c.logger.WithField("command", "sdl to").Debug(fmt.Sprintf("output: %s", stdout.String()))
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

// ConvertSDLToMetadata converts graphql SDL to hasura metadata
func (c *Config) ConvertSDLToMetadata(fromPayload types.SDLFromRequest) (fromResponse types.SDLFromResponse, err error) {
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
	sdlFromCmd := exec.Command(c.binPath)
	args := []string{"sdl", "from", "--input-file", inputFileName, "--output-file", outputFileName}
	sdlFromCmd.Args = append(sdlFromCmd.Args, args...)
	var stdout bytes.Buffer
	var stderr bytes.Buffer
	sdlFromCmd.Stdout = &stdout
	sdlFromCmd.Stderr = &stderr
	err = sdlFromCmd.Run()
	c.logger.WithField("command", "sdl from").Debugln(fmt.Sprintf("output: %s", stdout.String()))
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

// GetActionsCodegen generates codegen for an action
func (c *Config) GetActionsCodegen(codegenReq types.ActionsCodegenRequest) (codegenResp types.ActionsCodegenResponse, err error) {
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
	actionsCodegenCmd := exec.Command(c.binPath)
	args := []string{"actions-codegen", "--input-file", inputFileName, "--output-file", outputFileName}
	actionsCodegenCmd.Args = append(actionsCodegenCmd.Args, args...)
	var stdout bytes.Buffer
	var stderr bytes.Buffer
	actionsCodegenCmd.Stdout = &stdout
	actionsCodegenCmd.Stderr = &stderr
	err = actionsCodegenCmd.Run()
	c.logger.WithField("command", "actions-codegen").Debugln(fmt.Sprintf("output: %s", stdout.String()))
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
