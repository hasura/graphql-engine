package actions

import (
	"fmt"
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli"
	cliextension "github.com/hasura/graphql-engine/cli/metadata/actions/cli_extension"
	"github.com/hasura/graphql-engine/cli/metadata/actions/editor"
	"github.com/hasura/graphql-engine/cli/metadata/actions/types"
	"github.com/hasura/graphql-engine/cli/plugins"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/hasura/graphql-engine/cli/version"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"
)

const (
	actionsFileName string = "actions.yaml"
	graphqlFileName        = "actions.graphql"
	pluginName             = "cli-ext"
)

type ActionConfig struct {
	MetadataDir        string
	ActionConfig       *types.ActionExecutionConfig
	serverFeatureFlags *version.ServerFeatureFlags
	pluginsCfg         *plugins.Config
	cliExtensionConfig *cliextension.Config

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *ActionConfig {
	cfg := &ActionConfig{
		MetadataDir:        baseDir,
		ActionConfig:       ec.Config.ActionConfig,
		serverFeatureFlags: ec.Version.ServerFeatureFlags,
		logger:             ec.Logger,
		pluginsCfg:         ec.PluginsConfig,
		cliExtensionConfig: cliextension.NewCLIExtensionConfig(ec.PluginsConfig.Paths.BinPath(), ec.Logger),
	}
	return cfg
}

func (a *ActionConfig) Create(name string, introSchema interface{}, deriveFrom string) error {
	// Ensure CLI Extesnion
	err := a.ensureCLIExtension()
	if err != nil {
		return errors.Wrap(err, "error in install cli-extension plugin")
	}
	// Read the content of graphql file
	graphqlFileContent, err := a.GetActionsGraphQLFileContent()
	if err != nil {
		return errors.Wrapf(err, "error in reading %s file", graphqlFileName)
	}
	// Read actions.yaml
	oldAction, err := a.GetActionsFileContent()
	if err != nil {
		return errors.Wrapf(err, "error in reading %s file", actionsFileName)
	}
	// check if action already present
	for _, currAction := range oldAction.Actions {
		if currAction.Name == name {
			return fmt.Errorf("action %s already exists in %s", name, graphqlFileName)
		}
	}

	var defaultSDL string
	if introSchema == nil {
		defaultSDL = `type Mutation {
	# Define your action as a mutation here
	` + name + ` (arg1: SampleInput!): SampleOutput
}

type SampleOutput {
	accessToken: String!
}

input SampleInput {
	username: String!
	password: String!
}
`
	} else {
		sdlToReq := types.SDLToRequest{
			Derive: types.DerivePayload{
				IntrospectionSchema: introSchema,
				Operation:           deriveFrom,
				ActionName:          name,
			},
		}
		sdlToResp, err := a.cliExtensionConfig.ConvertMetadataToSDL(sdlToReq)
		if err != nil {
			return errors.Wrap(err, "error in converting metadata to sdl")
		}
		defaultSDL = sdlToResp.SDL.Complete
	}
	graphqlFileContent = defaultSDL + "\n" + graphqlFileContent
	data, err := editor.CaptureInputFromEditor(editor.GetPreferredEditorFromEnvironment, graphqlFileContent)
	if err != nil {
		return errors.Wrap(err, "error in getting input from editor")
	}
	sdlFromReq := types.SDLFromRequest{
		SDL: types.SDLPayload{
			Complete: string(data),
		},
	}
	sdlFromResp, err := a.cliExtensionConfig.ConvertSDLToMetadata(sdlFromReq)
	if err != nil {
		return errors.Wrap(err, "error in converting sdl to metadata")
	}
	currentActionNames := make([]string, 0)
	for actionIndex, action := range sdlFromResp.Actions {
		for _, currAction := range currentActionNames {
			if currAction == action.Name {
				return fmt.Errorf("action %s already exists in %s", action.Name, graphqlFileName)
			}
		}
		currentActionNames = append(currentActionNames, action.Name)
		for oldActionIndex, oldActionObj := range oldAction.Actions {
			if action.Name == oldActionObj.Name {
				sdlFromResp.Actions[actionIndex].Permissions = oldAction.Actions[oldActionIndex].Permissions
				sdlFromResp.Actions[actionIndex].Definition.Kind = oldAction.Actions[oldActionIndex].Definition.Kind
				sdlFromResp.Actions[actionIndex].Definition.Type = oldAction.Actions[oldActionIndex].Definition.Type
				sdlFromResp.Actions[actionIndex].Definition.Handler = oldAction.Actions[oldActionIndex].Definition.Handler
				sdlFromResp.Actions[actionIndex].Definition.ForwardClientHeaders = oldAction.Actions[oldActionIndex].Definition.ForwardClientHeaders
				sdlFromResp.Actions[actionIndex].Definition.Headers = oldAction.Actions[oldActionIndex].Definition.Headers
				break
			}
		}
		// Set kind and handler for action definition
		if sdlFromResp.Actions[actionIndex].Definition.Kind == "" {
			sdlFromResp.Actions[actionIndex].Definition.Kind = a.ActionConfig.Kind
		}
		if sdlFromResp.Actions[actionIndex].Definition.Handler == "" {
			sdlFromResp.Actions[actionIndex].Definition.Handler = a.ActionConfig.HandlerWebhookBaseURL + "/" + action.Name
		}
	}
	for customTypeIndex, customType := range sdlFromResp.Types.Enums {
		for oldTypeObjIndex, oldTypeObj := range oldAction.CustomTypes.Enums {
			if customType.Name == oldTypeObj.Name {
				sdlFromResp.Types.Enums[customTypeIndex].Description = oldAction.CustomTypes.Enums[oldTypeObjIndex].Description
				sdlFromResp.Types.Enums[customTypeIndex].Relationships = oldAction.CustomTypes.Enums[oldTypeObjIndex].Relationships
				break
			}
		}
	}
	for customTypeIndex, customType := range sdlFromResp.Types.InputObjects {
		for oldTypeObjIndex, oldTypeObj := range oldAction.CustomTypes.InputObjects {
			if customType.Name == oldTypeObj.Name {
				sdlFromResp.Types.InputObjects[customTypeIndex].Description = oldAction.CustomTypes.InputObjects[oldTypeObjIndex].Description
				sdlFromResp.Types.InputObjects[customTypeIndex].Relationships = oldAction.CustomTypes.InputObjects[oldTypeObjIndex].Relationships
				break
			}
		}
	}
	for customTypeIndex, customType := range sdlFromResp.Types.Objects {
		for oldTypeObjIndex, oldTypeObj := range oldAction.CustomTypes.Objects {
			if customType.Name == oldTypeObj.Name {
				sdlFromResp.Types.Objects[customTypeIndex].Description = oldAction.CustomTypes.Objects[oldTypeObjIndex].Description
				sdlFromResp.Types.Objects[customTypeIndex].Relationships = oldAction.CustomTypes.Objects[oldTypeObjIndex].Relationships
				break
			}
		}
	}
	for customTypeIndex, customType := range sdlFromResp.Types.Scalars {
		for oldTypeObjIndex, oldTypeObj := range oldAction.CustomTypes.Scalars {
			if customType.Name == oldTypeObj.Name {
				sdlFromResp.Types.Scalars[customTypeIndex].Description = oldAction.CustomTypes.Scalars[oldTypeObjIndex].Description
				sdlFromResp.Types.Scalars[customTypeIndex].Relationships = oldAction.CustomTypes.Scalars[oldTypeObjIndex].Relationships
				break
			}
		}
	}
	var common types.Common
	common.Actions = sdlFromResp.Actions
	common.CustomTypes = sdlFromResp.Types
	common.SetExportDefault()
	// write actions.yaml
	commonByt, err := yaml.Marshal(common)
	if err != nil {
		return errors.Wrap(err, "error in marshalling common")
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, actionsFileName), commonByt, 0644)
	if err != nil {
		return errors.Wrapf(err, "error in writing %s file", actionsFileName)
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, graphqlFileName), data, 0644)
	if err != nil {
		return errors.Wrapf(err, "error in writing %s file", graphqlFileName)
	}
	return nil
}

func (a *ActionConfig) Codegen(name string, derivePld types.DerivePayload) error {
	err := a.ensureCLIExtension()
	if err != nil {
		return errors.Wrap(err, "error in install cli-extension plugin")
	}

	graphqlFileContent, err := a.GetActionsGraphQLFileContent()
	if err != nil {
		return errors.Wrapf(err, "error in reading %s file", graphqlFileName)
	}
	data := types.ActionsCodegenRequest{
		ActionName: name,
		SDL: types.SDLPayload{
			Complete: graphqlFileContent,
		},
		CodegenConfig: a.ActionConfig.Codegen,
		Derive:        derivePld,
	}
	if a.ActionConfig.Codegen.URI == "" {
		data.CodegenConfig.URI = a.getActionsCodegenURI(data.CodegenConfig.Framework)
	}
	resp, err := a.cliExtensionConfig.GetActionsCodegen(data)
	if err != nil {
		return errors.Wrapf(err, "error in getting codegen for action %s", data.ActionName)
	}
	for _, file := range resp.Files {
		err = ioutil.WriteFile(filepath.Join(a.ActionConfig.Codegen.OutputDir, file.Name), []byte(file.Content), 0644)
		if err != nil {
			return errors.Wrap(err, "error in writing codegen file")
		}
	}
	return nil
}

func (a *ActionConfig) Validate() error {
	return nil
}

func (a *ActionConfig) CreateFiles() error {
	var common types.Common
	data, err := yaml.Marshal(common)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, actionsFileName), data, 0644)
	if err != nil {
		return err
	}
	graphqQLData := []byte(``)
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, graphqlFileName), graphqQLData, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (a *ActionConfig) Build(metadata *yaml.MapSlice) error {
	if !a.serverFeatureFlags.HasAction {
		_, err := a.GetActionsFileContent()
		if err == nil {
			a.logger.WithField("metadata_plugin", "actions").Warnf("Skipping building %s", actionsFileName)
		}
		_, err = a.GetActionsGraphQLFileContent()
		if err == nil {
			a.logger.WithField("metadata_plugin", "actions").Warnf("Skipping building %s", graphqlFileName)
		}
		return nil
	}
	err := a.ensureCLIExtension()
	if err != nil {
		return errors.Wrap(err, "error in install cli-extension plugin")
	}
	// Read actions.graphql
	graphqlFileContent, err := a.GetActionsGraphQLFileContent()
	if err != nil {
		return errors.Wrapf(err, "error in reading %s file", graphqlFileName)
	}

	sdlFromReq := types.SDLFromRequest{
		SDL: types.SDLPayload{
			Complete: graphqlFileContent,
		},
	}
	sdlFromResp, err := a.cliExtensionConfig.ConvertSDLToMetadata(sdlFromReq)
	if err != nil {
		return errors.Wrap(err, "error in converting sdl to metadata")
	}

	// Read actions.yaml
	oldAction, err := a.GetActionsFileContent()
	if err != nil {
		return errors.Wrapf(err, "error in reading %s", actionsFileName)
	}
	for actionIndex, action := range oldAction.Actions {
		var isFound bool
		for newActionIndex, newActionObj := range sdlFromResp.Actions {
			if action.Name == newActionObj.Name {
				isFound = true
				sdlFromResp.Actions[newActionIndex].Permissions = oldAction.Actions[actionIndex].Permissions
				sdlFromResp.Actions[newActionIndex].Definition.Kind = oldAction.Actions[actionIndex].Definition.Kind
				sdlFromResp.Actions[newActionIndex].Definition.Handler = oldAction.Actions[actionIndex].Definition.Handler
				sdlFromResp.Actions[newActionIndex].Definition.ForwardClientHeaders = oldAction.Actions[actionIndex].Definition.ForwardClientHeaders
				sdlFromResp.Actions[newActionIndex].Definition.Headers = oldAction.Actions[actionIndex].Definition.Headers
				break
			}
		}
		if !isFound {
			return fmt.Errorf("action %s is not present in %s", action.Name, graphqlFileName)
		}
	}
	for customTypeIndex, customType := range oldAction.CustomTypes.Enums {
		var isFound bool
		for newTypeObjIndex, newTypeObj := range sdlFromResp.Types.Enums {
			if customType.Name == newTypeObj.Name {
				isFound = true
				sdlFromResp.Types.Enums[newTypeObjIndex].Description = oldAction.CustomTypes.Enums[customTypeIndex].Description
				sdlFromResp.Types.Enums[newTypeObjIndex].Relationships = oldAction.CustomTypes.Enums[customTypeIndex].Relationships
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)
		}
	}
	for customTypeIndex, customType := range oldAction.CustomTypes.InputObjects {
		var isFound bool
		for newTypeObjIndex, newTypeObj := range sdlFromResp.Types.InputObjects {
			if customType.Name == newTypeObj.Name {
				isFound = true
				sdlFromResp.Types.InputObjects[newTypeObjIndex].Description = oldAction.CustomTypes.InputObjects[customTypeIndex].Description
				sdlFromResp.Types.InputObjects[newTypeObjIndex].Relationships = oldAction.CustomTypes.InputObjects[customTypeIndex].Relationships
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)
		}
	}
	for customTypeIndex, customType := range oldAction.CustomTypes.Objects {
		var isFound bool
		for newTypeObjIndex, newTypeObj := range sdlFromResp.Types.Objects {
			if customType.Name == newTypeObj.Name {
				isFound = true
				sdlFromResp.Types.Objects[newTypeObjIndex].Description = oldAction.CustomTypes.Objects[customTypeIndex].Description
				sdlFromResp.Types.Objects[newTypeObjIndex].Relationships = oldAction.CustomTypes.Objects[customTypeIndex].Relationships
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)
		}
	}
	for customTypeIndex, customType := range oldAction.CustomTypes.Scalars {
		var isFound bool
		for newTypeObjIndex, newTypeObj := range sdlFromResp.Types.Scalars {
			if customType.Name == newTypeObj.Name {
				isFound = true
				sdlFromResp.Types.Scalars[newTypeObjIndex].Description = oldAction.CustomTypes.Scalars[customTypeIndex].Description
				sdlFromResp.Types.Scalars[newTypeObjIndex].Relationships = oldAction.CustomTypes.Scalars[customTypeIndex].Relationships
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)
		}
	}
	if len(sdlFromResp.Actions) != 0 {
		actionItem := yaml.MapItem{
			Key:   "actions",
			Value: sdlFromResp.Actions,
		}
		*metadata = append(*metadata, actionItem)
	}
	customTypesLen := len(sdlFromResp.Types.Enums) + len(sdlFromResp.Types.InputObjects) + len(sdlFromResp.Types.Objects) + len(sdlFromResp.Types.Scalars)
	if customTypesLen != 0 {
		customTypeItem := yaml.MapItem{
			Key:   "custom_types",
			Value: sdlFromResp.Types,
		}
		*metadata = append(*metadata, customTypeItem)
	}
	return nil
}

func (a *ActionConfig) Export(metadata yaml.MapSlice) (map[string][]byte, error) {
	if !a.serverFeatureFlags.HasAction {
		a.logger.Debugf("Skipping creating %s and %s", actionsFileName, graphqlFileName)
		return make(map[string][]byte), nil
	}
	err := a.ensureCLIExtension()
	if err != nil {
		return nil, errors.Wrap(err, "error in install cli-extension plugin")
	}
	var actions yaml.MapSlice
	for _, item := range metadata {
		k, ok := item.Key.(string)
		if !ok || (k != "actions" && k != "custom_types") {
			continue
		}
		actions = append(actions, item)
	}
	ymlByt, err := yaml.Marshal(actions)
	if err != nil {
		return nil, errors.Wrap(err, "error in marshalling actions, custom_types from metadata")
	}
	var common types.Common
	err = yaml.Unmarshal(ymlByt, &common)
	if err != nil {
		return nil, errors.Wrap(err, "error in unmarshal to common")
	}
	var sdlToReq types.SDLToRequest
	sdlToReq.Types = common.CustomTypes
	sdlToReq.Actions = common.Actions
	sdlToResp, err := a.cliExtensionConfig.ConvertMetadataToSDL(sdlToReq)
	if err != nil {
		return nil, errors.Wrap(err, "error in converting metadata to sdl")
	}
	common.SetExportDefault()
	commonByt, err := yaml.Marshal(common)
	if err != nil {
		return nil, errors.Wrap(err, "error in marshaling common")
	}
	return map[string][]byte{
		filepath.Join(a.MetadataDir, actionsFileName): commonByt,
		filepath.Join(a.MetadataDir, graphqlFileName): []byte(sdlToResp.SDL.Complete),
	}, nil
}

func (a *ActionConfig) Name() string {
	return "actions"
}

func (a *ActionConfig) ensureCLIExtension() error {
	err := a.pluginsCfg.Install(pluginName, "", nil)
	if err != nil && err != plugins.ErrIsAlreadyInstalled {
		msg := fmt.Sprintf(`unable to install cli-ext plugin. execute the following commands to continue:

  hasura plugins install %s
`, pluginName)
		a.logger.Info(msg)
		return errors.Wrap(err, "cannot install cli-ext plugin")
	}
	return nil
}

func (a *ActionConfig) GetActionsFileContent() (content types.Common, err error) {
	commonByt, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, actionsFileName))
	if err != nil {
		return
	}
	err = yaml.Unmarshal(commonByt, &content)
	return
}

func (a *ActionConfig) GetActionsGraphQLFileContent() (sdl string, err error) {
	commonByt, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, graphqlFileName))
	if err != nil {
		return
	}
	sdl = string(commonByt)
	return
}

func (a *ActionConfig) getActionsCodegenURI(framework string) string {
	return fmt.Sprintf(`https://raw.githubusercontent.com/%s/master/%s/actions-codegen.js`, util.ActionsCodegenOrg, framework)
}
