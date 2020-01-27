package actions

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"path/filepath"

	"github.com/briandowns/spinner"
	"github.com/pkg/errors"

	"github.com/Masterminds/semver"
	gyaml "github.com/ghodss/yaml"
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
	dbTypes "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
)

const (
	actionsFileName       string = "actions.yaml"
	graphqlFileName              = "actions.graphql"
	actionsCodegenRepo           = "wawhal/actions-codegen"
	ActionsCodegenDirName        = "actions-codegen-assets"
)

var (
	ActionsCodegenRepoURI = fmt.Sprintf("https://github.com/%s.git", actionsCodegenRepo)
	pluginName            = "cli-ext"
)

type DeriveMutationPayload struct {
	MutationName string `json:"name"`
	ActionName   string `json:"action_name"`
}

type DerivePayload struct {
	IntrospectionSchema interface{}           `json:"introspection_schema"`
	Mutation            DeriveMutationPayload `json:"mutation"`
}

type sdlPayload struct {
	Complete string `json:"complete"`
}

type sdlToRequest struct {
	Types   types.CustomTypes `json:"types,omitempty"`
	Actions []types.Action    `json:"actions,omitempty"`
	Derive  DerivePayload     `json:"derive,omitempty"`
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
	ActionName    string                      `json:"action_name"`
	SDL           sdlPayload                  `json:"sdl"`
	Derive        DerivePayload               `json:"derive,omitempty"`
	CodegenConfig *cli.CodegenExecutionConfig `json:"codegen_config"`
}

type codegenFile struct {
	Name    string `json:"name"`
	Content string `json:"content"`
}

type actionsCodegenResponse struct {
	Files []codegenFile `json:"codegen"`
}

type ActionConfig struct {
	MetadataDir  string
	ActionConfig cli.ActionExecutionConfig
	cmdName      string
	shouldSkip   bool

	spinner *spinner.Spinner
	logger  *logrus.Logger
}

type OverrideOptions struct {
	Kind    string
	Webhook string
}

func New(ec *cli.ExecutionContext, opts *OverrideOptions) *ActionConfig {
	var shouldSkip bool
	if ec.Version.ServerSemver != nil {
		cons, err := semver.NewConstraint(">= v1.1.0")
		if err != nil {
			panic(err)
		}
		shouldSkip = !cons.Check(ec.Version.ServerSemver)
	}
	if !shouldSkip {
		err := ensureCLIExtension(ec.Plugins)
		if err != nil {
			ec.Spinner.Stop()
			ec.Logger.Errorln(err)
			msg := fmt.Sprintf(`unable to install cli-ext plugin. execute the following commands to continue:

  hasura plugins install %s
`, pluginName)
			ec.Logger.Fatalln(msg)
			return nil
		}
	}
	cfg := &ActionConfig{
		MetadataDir:  ec.MetadataDir,
		ActionConfig: ec.Config.Action,
		cmdName:      ec.CMDName,
		shouldSkip:   shouldSkip,
		spinner:      ec.Spinner,
		logger:       ec.Logger,
	}
	if opts != nil {
		if opts.Kind != "" {
			cfg.ActionConfig.Kind = opts.Kind
		}

		if opts.Webhook != "" {
			cfg.ActionConfig.HandlerWebhookBaseURL = opts.Webhook
		}
	}
	return cfg
}

func (a *ActionConfig) Create(name string, introSchema interface{}, deriveFromMutation string) error {
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
				Mutation: DeriveMutationPayload{
					MutationName: deriveFromMutation,
					ActionName:   name,
				},
			},
		}
		sdlToResp, err := convertMetadataToSDL(sdlToReq, a.cmdName)
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
	sdlFromResp, err := convertSDLToMetadata(sdlFromReq, a.cmdName)
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
		if action.Definition.Kind == "" {
			sdlFromResp.Actions[actionIndex].Definition.Kind = a.ActionConfig.Kind
		}
		if action.Definition.Handler == "" {
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
	resp, err := getActionsCodegen(data, a.cmdName)
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

func (a *ActionConfig) Build(metadata *dbTypes.Metadata) error {
	if a.shouldSkip {
		_, err := GetActionsFileContent(a.MetadataDir)
		if err == nil {
			a.spinner.Stop()
			a.logger.WithField("metadata_plugin", "actions").Warnf("Skipping building %s", actionsFileName)
			a.spinner.Start()
		}
		_, err = GetActionsGraphQLFileContent(a.MetadataDir)
		if err == nil {
			a.spinner.Stop()
			a.logger.WithField("metadata_plugin", "actions").Warnf("Skipping building %s", graphqlFileName)
			a.spinner.Start()
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

	sdlFromResp, err := convertSDLToMetadata(sdlFromReq, a.cmdName)
	if err != nil {
		return err
	}

	// Read actions.yaml
	oldAction, err := GetActionsFileContent(a.MetadataDir)
	if err != nil {
		return err
	}
	for _, action := range oldAction.Actions {
		var isFound bool
		for _, newActionObj := range sdlFromResp.Actions {
			if action.Name == newActionObj.Name {
				isFound = true
				break
			}
		}
		if !isFound {
			return fmt.Errorf("action %s is not present in %s", action.Name, graphqlFileName)
		}
	}
	for _, customType := range oldAction.CustomTypes.Enums {
		var isFound bool
		for _, newTypeObj := range sdlFromResp.Types.Enums {
			if customType.Name == newTypeObj.Name {
				isFound = true
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)
		}
	}
	for _, customType := range oldAction.CustomTypes.InputObjects {
		var isFound bool
		for _, newTypeObj := range sdlFromResp.Types.InputObjects {
			if customType.Name == newTypeObj.Name {
				isFound = true
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)
		}
	}
	for _, customType := range oldAction.CustomTypes.Objects {
		var isFound bool
		for _, newTypeObj := range sdlFromResp.Types.Objects {
			if customType.Name == newTypeObj.Name {
				isFound = true
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)
		}
	}
	for _, customType := range oldAction.CustomTypes.Scalars {
		var isFound bool
		for _, newTypeObj := range sdlFromResp.Types.Scalars {
			if customType.Name == newTypeObj.Name {
				isFound = true
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
	metadata.Actions = sdlFromResp.Actions
	metadata.CustomTypes = sdlFromResp.Types
	return nil
}

func (a *ActionConfig) Export(metadata dbTypes.Metadata) error {
	if a.shouldSkip {
		a.logger.Debugf("Skipping creating %s and %s", actionsFileName, graphqlFileName)
		return nil
	}
	tmpByt, err := json.Marshal(metadata)
	if err != nil {
		return err
	}
	var common types.Common
	err = json.Unmarshal(tmpByt, &common)
	if err != nil {
		return err
	}
	var sdlToReq sdlToRequest
	sdlToReq.Types = common.CustomTypes
	sdlToReq.Actions = common.Actions
	sdlToResp, err := convertMetadataToSDL(sdlToReq, a.cmdName)
	if err != nil {
		return err
	}
	doc, err := parser.Parse(parser.ParseParams{
		Source: sdlToResp.SDL.Complete,
	})
	if err != nil {
		return err
	}
	commonByt, err := yaml.Marshal(common)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, actionsFileName), commonByt, 0644)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, graphqlFileName), []byte(printer.Print(doc).(string)), 0644)
	if err != nil {
		return err
	}
	return nil
}

func GetActionsFileContent(metadataDir string) (content types.Common, err error) {
	commonByt, err := ioutil.ReadFile(filepath.Join(metadataDir, actionsFileName))
	if err != nil {
		return
	}
	err = gyaml.Unmarshal(commonByt, &content)
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

func ensureCLIExtension(pluginCfg *plugins.Config) error {
	err := pluginCfg.Install(pluginName)
	if err != nil && err != plugins.ErrIsAlreadyInstalled {
		return errors.Wrap(err, "cannot install cli-ext plugin")
	}
	return nil
}
