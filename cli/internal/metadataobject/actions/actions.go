package actions

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/cliext"
	cliextension "github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions/cli_extension"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions/editor"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject/actions/types"
	"github.com/hasura/graphql-engine/cli/v2/util"
	"github.com/hasura/graphql-engine/cli/v2/version"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v3"
)

const (
	graphqlFileName = "actions.graphql"
)

type ActionConfig struct {
	MetadataDir        string
	ActionConfig       *types.ActionExecutionConfig
	serverFeatureFlags *version.ServerFeatureFlags
	cliExtensionConfig *cliextension.Config
	ensureCliExt       func() error
	cleanupCliExt      func()

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *ActionConfig {
	cfg := &ActionConfig{
		MetadataDir:        baseDir,
		ActionConfig:       ec.Config.ActionConfig,
		serverFeatureFlags: ec.Version.ServerFeatureFlags,
		logger:             ec.Logger,
		cliExtensionConfig: cliextension.NewCLIExtensionConfig(&ec.CliExtDestinationBinPath, ec.Logger),
		ensureCliExt: func() error {
			var op errors.Op = "actions.New.ensureCliExt"
			err := cliext.Setup(ec)
			if err != nil {
				return errors.E(op, err)
			}
			return nil
		},
		cleanupCliExt: func() {
			cliext.Cleanup(ec)
		},
	}
	return cfg
}

func (a *ActionConfig) Create(name string, introSchema interface{}, deriveFrom string) error {
	var op errors.Op = "actions.ActionConfig.Create"
	err := a.ensureCliExt()
	defer a.cleanupCliExt()
	if err != nil {
		return errors.E(op, err)
	}

	// Read the content of graphql file
	graphqlFileContent, err := a.GetActionsGraphQLFileContent()
	if err != nil {
		return errors.E(op, fmt.Errorf("error in reading %s file: %w", graphqlFileName, err))
	}
	// Read actions.yaml
	oldAction, err := a.GetActionsFileContent()
	if err != nil {
		return errors.E(op, fmt.Errorf("error in reading %s file: %w", a.Filename(), err))
	}
	// check if action already present
	for _, currAction := range oldAction.Actions {
		if currAction.Name == name {
			return errors.E(op, fmt.Errorf("action %s already exists in %s", name, graphqlFileName))
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
			return errors.E(op, fmt.Errorf("error in converting metadata to sdl: %w", err))
		}
		defaultSDL = sdlToResp.SDL.Complete
	}
	graphqlFileContent = defaultSDL + "\n" + graphqlFileContent
	data, err := editor.CaptureInputFromEditor(editor.GetPreferredEditorFromEnvironment, graphqlFileContent, "graphql")
	if err != nil {
		return errors.E(op, fmt.Errorf("error in getting input from editor: %w", err))
	}
	sdlFromReq := types.SDLFromRequest{
		SDL: types.SDLPayload{
			Complete: string(data),
		},
	}
	sdlFromResp, err := a.cliExtensionConfig.ConvertSDLToMetadata(sdlFromReq)
	if err != nil {
		return errors.E(op, fmt.Errorf("error in converting sdl to metadata: %w", err))
	}
	currentActionNames := make([]string, 0)
	for actionIndex, action := range sdlFromResp.Actions {
		for _, currAction := range currentActionNames {
			if currAction == action.Name {
				return errors.E(op, fmt.Errorf("action %s already exists in %s", action.Name, graphqlFileName))
			}
		}
		currentActionNames = append(currentActionNames, action.Name)
		for oldActionIndex, oldActionObj := range oldAction.Actions {
			if action.Name == oldActionObj.Name {
				sdlFromResp.Actions[actionIndex].Permissions = oldAction.Actions[oldActionIndex].Permissions
				sdlFromResp.Actions[actionIndex].Comment = oldAction.Actions[oldActionIndex].Comment
				sdlFromResp.Actions[actionIndex].Definition.Timeout = oldAction.Actions[oldActionIndex].Definition.Timeout
				sdlFromResp.Actions[actionIndex].Definition.Kind = oldAction.Actions[oldActionIndex].Definition.Kind
				sdlFromResp.Actions[actionIndex].Definition.Type = oldAction.Actions[oldActionIndex].Definition.Type
				sdlFromResp.Actions[actionIndex].Definition.Handler = oldAction.Actions[oldActionIndex].Definition.Handler
				sdlFromResp.Actions[actionIndex].Definition.ForwardClientHeaders = oldAction.Actions[oldActionIndex].Definition.ForwardClientHeaders
				sdlFromResp.Actions[actionIndex].Definition.Headers = oldAction.Actions[oldActionIndex].Definition.Headers
				sdlFromResp.Actions[actionIndex].Definition.RequestTransform = oldAction.Actions[oldActionIndex].Definition.RequestTransform
				sdlFromResp.Actions[actionIndex].Definition.ResponseTransform = oldAction.Actions[oldActionIndex].Definition.ResponseTransform
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
		return errors.E(op, fmt.Errorf("error in marshalling common: %w", err))
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, a.Filename()), commonByt, 0644)
	if err != nil {
		return errors.E(op, fmt.Errorf("error in writing %s file: %w", a.Filename(), err))
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, graphqlFileName), data, 0644)
	if err != nil {
		return errors.E(op, fmt.Errorf("error in writing %s file: %w", graphqlFileName, err))
	}
	return nil
}

func (a *ActionConfig) Codegen(name string, derivePld types.DerivePayload) error {
	var op errors.Op = "actions.ActionConfig.Codegen"
	err := a.ensureCliExt()
	defer a.cleanupCliExt()
	if err != nil {
		return errors.E(op, err)
	}

	graphqlFileContent, err := a.GetActionsGraphQLFileContent()
	if err != nil {
		return errors.E(op, fmt.Errorf("error in reading %s file: %w", graphqlFileName, err))
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
		return errors.E(op, fmt.Errorf("error in getting codegen for action %s: %w", data.ActionName, err))
	}
	for _, file := range resp.Files {
		err = ioutil.WriteFile(filepath.Join(a.ActionConfig.Codegen.OutputDir, file.Name), []byte(file.Content), 0644)
		if err != nil {
			return errors.E(op, fmt.Errorf("error in writing codegen file: %w", err))
		}
	}
	return nil
}

func (a *ActionConfig) Validate() error {
	return nil
}

func (a *ActionConfig) CreateFiles() error {
	var op errors.Op = "actions.ActionConfig.CreateFiles"
	var common types.Common
	data, err := yaml.Marshal(common)
	if err != nil {
		return errors.E(op, err)
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, a.Filename()), data, 0644)
	if err != nil {
		return errors.E(op, err)
	}
	graphqQLData := []byte(``)
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, graphqlFileName), graphqQLData, 0644)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (a *ActionConfig) Build() (map[string]interface{}, error) {
	var op errors.Op = "actions.ActionConfig.Build"
	if !a.serverFeatureFlags.HasAction {
		_, err := a.GetActionsFileContent()
		if err == nil {
			a.logger.WithField("metadata_plugin", "actions").Warnf("Skipping building %s", a.Filename())
		}
		_, err = a.GetActionsGraphQLFileContent()
		if err == nil {
			a.logger.WithField("metadata_plugin", "actions").Warnf("Skipping building %s", graphqlFileName)
		}
		return nil, nil
	}
	err := a.ensureCliExt()
	defer a.cleanupCliExt()
	if err != nil {
		return nil, errors.E(op, a.error(err))
	}
	// Read actions.graphql
	graphqlFileContent, err := a.GetActionsGraphQLFileContent()
	if err != nil {
		return nil, errors.E(op, a.error(fmt.Errorf("error in reading %s file: %w", graphqlFileName, err)))
	}

	sdlFromReq := types.SDLFromRequest{
		SDL: types.SDLPayload{
			Complete: graphqlFileContent,
		},
	}
	sdlFromResp, err := a.cliExtensionConfig.ConvertSDLToMetadata(sdlFromReq)
	if err != nil {
		return nil, errors.E(op, errors.KindBadInput, a.error(fmt.Errorf("error in converting sdl to metadata: %w", err)))
	}

	// Read actions.yaml
	oldAction, err := a.GetActionsFileContent()
	if err != nil {
		return nil, errors.E(op, a.error(fmt.Errorf("error in reading %s: %w", a.Filename(), err)))
	}
	for actionIndex, action := range oldAction.Actions {
		var isFound bool
		for newActionIndex, newActionObj := range sdlFromResp.Actions {
			if action.Name == newActionObj.Name {
				isFound = true
				sdlFromResp.Actions[newActionIndex].Permissions = oldAction.Actions[actionIndex].Permissions
				sdlFromResp.Actions[newActionIndex].Comment = oldAction.Actions[actionIndex].Comment
				sdlFromResp.Actions[newActionIndex].Definition.Timeout = oldAction.Actions[actionIndex].Definition.Timeout
				sdlFromResp.Actions[newActionIndex].Definition.Kind = oldAction.Actions[actionIndex].Definition.Kind
				sdlFromResp.Actions[newActionIndex].Definition.Handler = oldAction.Actions[actionIndex].Definition.Handler
				sdlFromResp.Actions[newActionIndex].Definition.ForwardClientHeaders = oldAction.Actions[actionIndex].Definition.ForwardClientHeaders
				sdlFromResp.Actions[newActionIndex].Definition.Headers = oldAction.Actions[actionIndex].Definition.Headers
				sdlFromResp.Actions[newActionIndex].Definition.RequestTransform = oldAction.Actions[actionIndex].Definition.RequestTransform
				sdlFromResp.Actions[newActionIndex].Definition.ResponseTransform = oldAction.Actions[actionIndex].Definition.ResponseTransform
				break
			}
		}
		if !isFound {
			return nil, errors.E(op, errors.KindBadInput, a.error(fmt.Errorf("action %s is not present in %s", action.Name, graphqlFileName)))
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
			return nil, errors.E(op, errors.KindBadInput, a.error(fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)))
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
			return nil, errors.E(op, errors.KindBadInput, a.error(fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)))
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
			return nil, errors.E(op, errors.KindBadInput, a.error(fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)))
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
			return nil, errors.E(op, errors.KindBadInput, a.error(fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)))
		}
	}
	metadata := map[string]interface{}{}
	if len(sdlFromResp.Actions) != 0 {
		metadata[metadataobject.ActionsKey] = sdlFromResp.Actions
	}
	customTypesLen := len(sdlFromResp.Types.Enums) + len(sdlFromResp.Types.InputObjects) + len(sdlFromResp.Types.Objects) + len(sdlFromResp.Types.Scalars)
	if customTypesLen != 0 {
		metadata[metadataobject.CustomTypesKey] = sdlFromResp.Types
	}
	return metadata, nil
}

func (a *ActionConfig) Export(metadata map[string]yaml.Node) (map[string][]byte, error) {
	var op errors.Op = "actions.ActionConfig.Export"
	if !a.serverFeatureFlags.HasAction {
		a.logger.Debugf("Skipping creating %s and %s", a.Filename(), graphqlFileName)
		return make(map[string][]byte), nil
	}
	err := a.ensureCliExt()
	defer a.cleanupCliExt()
	if err != nil {
		return nil, errors.E(op, a.error(err))
	}
	actions := map[string]yaml.Node{}
	if v, ok := metadata[metadataobject.ActionsKey]; ok {
		actions[metadataobject.ActionsKey] = v
	}
	if v, ok := metadata[metadataobject.CustomTypesKey]; ok {
		actions[metadataobject.CustomTypesKey] = v
	}

	ymlByt := new(bytes.Buffer)
	if err := metadataobject.GetEncoder(ymlByt).Encode(actions); err != nil {
		return nil, errors.E(op, a.error(fmt.Errorf("error in marshalling actions, custom_types from metadata: %w", err)))
	}
	var common types.Common
	err = yaml.NewDecoder(ymlByt).Decode(&common)
	if err != nil {
		return nil, errors.E(op, a.error(fmt.Errorf("error in unmarshal to common: %w", err)))
	}
	var sdlToReq types.SDLToRequest
	sdlToReq.Types = common.CustomTypes
	sdlToReq.Actions = common.Actions
	sdlToResp, err := a.cliExtensionConfig.ConvertMetadataToSDL(sdlToReq)
	if err != nil {
		return nil, errors.E(op, a.error(fmt.Errorf("error in converting metadata to sdl: %w", err)))
	}
	common.SetExportDefault()
	commonByt := new(bytes.Buffer)
	if err = metadataobject.GetEncoder(commonByt).Encode(common); err != nil {
		return nil, errors.E(op, a.error(fmt.Errorf("error in marshaling common: %w", err)))
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(a.MetadataDir, a.Filename())):    commonByt.Bytes(),
		filepath.ToSlash(filepath.Join(a.MetadataDir, graphqlFileName)): []byte(sdlToResp.SDL.Complete),
	}, nil
}

func (a *ActionConfig) Key() string {
	return metadataobject.ActionsKey
}

func (a *ActionConfig) Filename() string {
	return "actions.yaml"
}

func (a *ActionConfig) GetFiles() ([]string, error) {
	var op errors.Op = "actions.ActionConfig.GetFiles"
	rootFile := filepath.Join(a.BaseDirectory(), a.Filename())
	files, err := metadataobject.DefaultGetFiles(rootFile)
	if err != nil {
		return nil, errors.E(op, a.error(err))
	}
	files = append(files, filepath.Join(a.BaseDirectory(), graphqlFileName))
	return files, nil
}

func (a *ActionConfig) WriteDiff(opts metadataobject.WriteDiffOpts) error {
	var op errors.Op = "actions.ActionConfig.WriteDiff"
	err := metadataobject.DefaultWriteDiff(metadataobject.DefaultWriteDiffOpts{From: a, WriteDiffOpts: opts})
	if err != nil {
		return errors.E(op, a.error(err))
	}
	return nil
}

func (a *ActionConfig) BaseDirectory() string {
	return a.MetadataDir
}

func (a *ActionConfig) GetActionsFileContent() (content types.Common, err error) {
	var op errors.Op = "actions.ActionConfig.GetActionsFileContent"
	commonByt, err := metadataobject.ReadMetadataFile(filepath.Join(a.MetadataDir, a.Filename()))
	if err != nil {
		err = errors.E(op, err)
		return
	}
	err = yaml.Unmarshal(commonByt, &content)
	if err != nil {
		err = errors.E(op, errors.KindBadInput, err)
		return
	}
	return
}

func (a *ActionConfig) GetActionsGraphQLFileContent() (sdl string, err error) {
	var op errors.Op = "actions.ActionConfig.GetActionsGraphQLFileContent"
	commonByt, err := metadataobject.ReadMetadataFile(filepath.Join(a.MetadataDir, graphqlFileName))
	if err != nil {
		err = errors.E(op, err)
		return
	}
	sdl = string(commonByt)
	return
}

func (a *ActionConfig) getActionsCodegenURI(framework string) string {
	return fmt.Sprintf(`https://raw.githubusercontent.com/%s/master/%s/actions-codegen.js`, util.ActionsCodegenOrg, framework)
}

func (a *ActionConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(a, err, additionalContext...)
}
