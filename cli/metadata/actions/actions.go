package actions

import (
	"fmt"
	"io/ioutil"
	"path/filepath"

	"github.com/pkg/errors"

	"github.com/Masterminds/semver"
	"github.com/graphql-go/graphql/language/ast"
	"github.com/graphql-go/graphql/language/kinds"
	"github.com/graphql-go/graphql/language/parser"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata/actions/printer"
	"github.com/hasura/graphql-engine/cli/plugins"
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"

	"github.com/hasura/graphql-engine/cli/metadata/actions/editor"
	"github.com/hasura/graphql-engine/cli/metadata/actions/types"
)

const (
	actionsFileName       string = "actions.yaml"
	graphqlFileName              = "actions.graphql"
	actionsCodegenRepo           = "hasura/codegen-assets"
	ActionsCodegenDirName        = "actions-codegen-assets"
)

var (
	ActionsCodegenRepoURI = fmt.Sprintf("https://github.com/%s.git", actionsCodegenRepo)
	pluginName            = "cli-ext"
)

type DerivePayload struct {
	IntrospectionSchema interface{} `json:"introspection_schema" yaml:"introspection_schema,omitempty"`
	Operation           string      `json:"operation" yaml:"operation,omitempty"`
	ActionName          string      `json:"action_name" yaml:"action_name,omitempty"`
}

type sdlPayload struct {
	Complete string `json:"complete"`
}

type sdlToRequest struct {
	Types   types.CustomTypes `json:"types,omitempty" yaml:"types,omitempty"`
	Actions []types.Action    `json:"actions,omitempty" yaml:"actions,omitempty"`
	Derive  DerivePayload     `json:"derive,omitempty" yaml:"derive,omitempty"`
}

type sdlToResponse struct {
	SDL sdlPayload `json:"sdl"`
}

type sdlFromRequest struct {
	SDL sdlPayload `json:"sdl"`
}

type sdlFromResponse struct {
	Types   types.CustomTypes `json:"types"`
	Actions []types.Action    `json:"actions"`
}

type actionsCodegenRequest struct {
	ActionName    string                        `json:"action_name" yaml:"action_name,omitempty"`
	SDL           sdlPayload                    `json:"sdl" yaml:"sdl,omitempty"`
	Derive        DerivePayload                 `json:"derive,omitempty"`
	CodegenConfig *types.CodegenExecutionConfig `json:"codegen_config" yaml:"codegen_config,omitempty"`
}

type codegenFile struct {
	Name    string `json:"name"`
	Content string `json:"content"`
}

type actionsCodegenResponse struct {
	Files []codegenFile `json:"codegen" yaml:"codegen,omitempty"`
}

type ActionConfig struct {
	MetadataDir  string
	ActionConfig types.ActionExecutionConfig

	cmdName    string
	shouldSkip bool
	pluginsCfg *plugins.Config

	logger *logrus.Logger
}

type OverrideOptions struct {
	Kind    string
	Webhook string
}

func New(ec *cli.ExecutionContext, baseDir string) *ActionConfig {
	var shouldSkip bool
	if ec.Version != nil && ec.Version.ServerSemver != nil {
		cons, err := semver.NewConstraint(">= v1.1.0")
		if err != nil {
			panic(err)
		}
		shouldSkip = !cons.Check(ec.Version.ServerSemver)
	}
	cfg := &ActionConfig{
		MetadataDir:  baseDir,
		ActionConfig: ec.Config.ActionConfig,
		cmdName:      ec.CMDName,
		shouldSkip:   shouldSkip,
		logger:       ec.Logger,
		pluginsCfg:   ec.PluginsConfig,
	}
	return cfg
}

func (a *ActionConfig) Create(name string, introSchema interface{}, deriveFrom string, opts *OverrideOptions) error {
	if !a.shouldSkip {
		err := a.ensureCLIExtension()
		if err != nil {
			return err
		}
	}
	graphqlFileContent, err := GetActionsGraphQLFileContent(a.MetadataDir)
	if err != nil {
		return err
	}

	doc, err := parser.Parse(parser.ParseParams{
		Source: graphqlFileContent,
	})

	if err != nil {
		return err
	}

	currentActionNames := make([]string, 0)
	// Check if the action already exists, if yes throw error
	for _, def := range doc.Definitions {
		switch obj := def.(type) {
		case *ast.ObjectDefinition:
			if obj.Kind == kinds.ObjectDefinition && obj.Name.Kind == kinds.Name && obj.Name.Value == "Mutation" {
				for _, field := range obj.Fields {
					currentActionNames = append(currentActionNames, field.Name.Value)
				}
			}
		case *ast.TypeExtensionDefinition:
			if obj.Kind == kinds.TypeExtensionDefinition && obj.Definition.Name.Kind == kinds.Name && obj.Definition.Name.Value == "Mutation" {
				for _, field := range obj.Definition.Fields {
					currentActionNames = append(currentActionNames, field.Name.Value)
				}
			}
		}
	}
	for _, currAction := range currentActionNames {
		if currAction == name {
			return fmt.Errorf("action %s already exists in %s", name, graphqlFileName)
		}
	}

	// add extend type mutation
	for index, def := range doc.Definitions {
		switch obj := def.(type) {
		case *ast.ObjectDefinition:
			if obj.Kind == kinds.ObjectDefinition && obj.Name.Kind == kinds.Name && obj.Name.Value == "Mutation" {
				newObj := &ast.TypeExtensionDefinition{
					Kind:       kinds.TypeExtensionDefinition,
					Definition: obj,
				}
				doc.Definitions[index] = newObj
			}
		}
	}

	var defaultSDL string
	if introSchema == nil {
		defaultSDL = `
extend type Mutation {
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
		sdlToReq := sdlToRequest{
			Derive: DerivePayload{
				IntrospectionSchema: introSchema,
				Operation:           deriveFrom,
				ActionName:          name,
			},
		}
		sdlToResp, err := convertMetadataToSDL(sdlToReq, a.cmdName, a.logger)
		if err != nil {
			return err
		}
		defaultSDL = sdlToResp.SDL.Complete
	}
	newDoc, err := parser.Parse(parser.ParseParams{
		Source: defaultSDL,
	})
	if err != nil {
		return err
	}
	doc.Definitions = append(newDoc.Definitions, doc.Definitions...)
	inputDupData := map[string][]int{}
	objDupData := map[string][]int{}
	for index, def := range doc.Definitions {
		switch obj := def.(type) {
		case *ast.ObjectDefinition:
			if obj.GetName().Value != "Mutation" {
				// check if name already exists
				_, ok := objDupData[obj.GetName().Value]
				if !ok {
					objDupData[obj.GetName().Value] = []int{index}
				} else {
					objDupData[obj.GetName().Value] = append(objDupData[obj.GetName().Value], index)
				}
			}
		case *ast.InputObjectDefinition:
			_, ok := inputDupData[obj.GetName().Value]
			if !ok {
				inputDupData[obj.GetName().Value] = []int{index}
			} else {
				inputDupData[obj.GetName().Value] = append(inputDupData[obj.GetName().Value], index)
			}
		}
	}
	for _, indexes := range inputDupData {
		if len(indexes) > 0 {
			indexes = indexes[:len(indexes)-1]
		}
		for _, index := range indexes {
			doc.Definitions = append(doc.Definitions[:index], doc.Definitions[index+1:]...)
		}
	}
	for _, indexes := range objDupData {
		if len(indexes) > 0 {
			indexes = indexes[:len(indexes)-1]
		}
		for _, index := range indexes {
			doc.Definitions = append(doc.Definitions[:index], doc.Definitions[index+1:]...)
		}
	}
	defaultText := printer.Print(doc).(string)
	data, err := editor.CaptureInputFromEditor(editor.GetPreferredEditorFromEnvironment, defaultText)
	if err != nil {
		return err
	}
	sdlFromReq := sdlFromRequest{
		SDL: sdlPayload{
			Complete: string(data),
		},
	}
	sdlFromResp, err := convertSDLToMetadata(sdlFromReq, a.cmdName, a.logger)
	if err != nil {
		return err
	}
	// Read actions.yaml
	oldAction, err := GetActionsFileContent(a.MetadataDir)
	if err != nil {
		return err
	}
	currentActionNames = make([]string, 0)
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
				sdlFromResp.Actions[actionIndex].Definition.Handler = oldAction.Actions[oldActionIndex].Definition.Handler
				break
			}
		}
		// Set kind and handler for action definition
		kind := a.ActionConfig.Kind
		handler := a.ActionConfig.HandlerWebhookBaseURL + "/" + action.Name
		if opts != nil {
			if opts.Kind != "" {
				kind = opts.Kind
			}

			if opts.Webhook != "" {
				handler = opts.Webhook
			}
		}
		if action.Definition.Kind == "" {
			sdlFromResp.Actions[actionIndex].Definition.Kind = kind
		}
		if action.Definition.Handler == "" {
			sdlFromResp.Actions[actionIndex].Definition.Handler = handler
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
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, actionsFileName), commonByt, 0644)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, graphqlFileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (a *ActionConfig) Codegen(name string, derivePld DerivePayload) error {
	if !a.shouldSkip {
		err := a.ensureCLIExtension()
		if err != nil {
			return err
		}
	}
	// Do nothing if the codegen framework does not exist
	if a.ActionConfig.Codegen.Framework == "" {
		return nil
	}

	graphqlFileContent, err := GetActionsGraphQLFileContent(a.MetadataDir)
	if err != nil {
		return err
	}
	data := actionsCodegenRequest{
		ActionName: name,
		SDL: sdlPayload{
			Complete: graphqlFileContent,
		},
		CodegenConfig: a.ActionConfig.Codegen,
		Derive:        derivePld,
	}
	if a.ActionConfig.Codegen.URI == "" {
		data.CodegenConfig.URI = getActionsCodegenURI(data.CodegenConfig.Framework)
	}
	resp, err := getActionsCodegen(data, a.cmdName, a.logger)
	if err != nil {
		return err
	}

	for _, file := range resp.Files {
		err = ioutil.WriteFile(filepath.Join(a.ActionConfig.Codegen.OutputDir, file.Name), []byte(file.Content), 0644)
		if err != nil {
			return err
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
	if !a.shouldSkip {
		err := a.ensureCLIExtension()
		if err != nil {
			return err
		}
	}
	if a.shouldSkip {
		_, err := GetActionsFileContent(a.MetadataDir)
		if err == nil {
			a.logger.WithField("metadata_plugin", "actions").Warnf("Skipping building %s", actionsFileName)
		}
		_, err = GetActionsGraphQLFileContent(a.MetadataDir)
		if err == nil {
			a.logger.WithField("metadata_plugin", "actions").Warnf("Skipping building %s", graphqlFileName)
		}
		return nil
	}
	// Read actions.graphql
	graphqlFileContent, err := GetActionsGraphQLFileContent(a.MetadataDir)
	if err != nil {
		return err
	}
	sdlFromReq := sdlFromRequest{
		SDL: sdlPayload{
			Complete: graphqlFileContent,
		},
	}

	sdlFromResp, err := convertSDLToMetadata(sdlFromReq, a.cmdName, a.logger)
	if err != nil {
		return err
	}

	// Read actions.yaml
	oldAction, err := GetActionsFileContent(a.MetadataDir)
	if err != nil {
		return err
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
				sdlFromResp.Types.Objects[newTypeObjIndex].Description = oldAction.CustomTypes.Objects[customTypeIndex].Description
				sdlFromResp.Types.Objects[newTypeObjIndex].Relationships = oldAction.CustomTypes.Objects[customTypeIndex].Relationships
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)
		}
	}
	for index, action := range sdlFromResp.Actions {
		sdlFromResp.Actions[index].Definition.Kind = a.ActionConfig.Kind
		sdlFromResp.Actions[index].Definition.Handler = a.ActionConfig.HandlerWebhookBaseURL + "/" + action.Name
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
	if !a.shouldSkip {
		err := a.ensureCLIExtension()
		if err != nil {
			return nil, err
		}
	}
	if a.shouldSkip {
		a.logger.Debugf("Skipping creating %s and %s", actionsFileName, graphqlFileName)
		return make(map[string][]byte), nil
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
		return nil, err
	}
	var common types.Common
	err = yaml.Unmarshal(ymlByt, &common)
	if err != nil {
		return nil, err
	}
	var sdlToReq sdlToRequest
	sdlToReq.Types = common.CustomTypes
	sdlToReq.Actions = common.Actions
	sdlToResp, err := convertMetadataToSDL(sdlToReq, a.cmdName, a.logger)
	if err != nil {
		return nil, err
	}
	doc, err := parser.Parse(parser.ParseParams{
		Source: sdlToResp.SDL.Complete,
	})
	if err != nil {
		return nil, err
	}
	common.SetExportDefault()
	commonByt, err := yaml.Marshal(common)
	if err != nil {
		return nil, err
	}
	return map[string][]byte{
		filepath.Join(a.MetadataDir, actionsFileName): commonByt,
		filepath.Join(a.MetadataDir, graphqlFileName): []byte(printer.Print(doc).(string)),
	}, nil
}

func (a *ActionConfig) Name() string {
	return "actions"
}

func (a *ActionConfig) ensureCLIExtension() error {
	err := a.pluginsCfg.Install(pluginName, "")
	if err != nil && err != plugins.ErrIsAlreadyInstalled {
		msg := fmt.Sprintf(`unable to install cli-ext plugin. execute the following commands to continue:

  hasura plugins install %s
`, pluginName)
		a.logger.Info(msg)
		return errors.Wrap(err, "cannot install cli-ext plugin")
	}
	return nil
}

func GetActionsFileContent(metadataDir string) (content types.Common, err error) {
	commonByt, err := ioutil.ReadFile(filepath.Join(metadataDir, actionsFileName))
	if err != nil {
		return
	}
	err = yaml.Unmarshal(commonByt, &content)
	return
}

func GetActionsGraphQLFileContent(metadataDir string) (sdl string, err error) {
	commonByt, err := ioutil.ReadFile(filepath.Join(metadataDir, graphqlFileName))
	if err != nil {
		return
	}
	sdl = string(commonByt)
	return
}
