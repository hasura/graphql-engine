package actions

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os/exec"
	"path/filepath"

	gyaml "github.com/ghodss/yaml"
	"github.com/graphql-go/graphql/language/ast"
	"github.com/graphql-go/graphql/language/kinds"
	"github.com/graphql-go/graphql/language/parser"
	"github.com/hasura/graphql-engine/cli/metadata/actions/printer"
	"gopkg.in/yaml.v2"

	"github.com/hasura/graphql-engine/cli/metadata/actions/editor"
	"github.com/hasura/graphql-engine/cli/metadata/actions/types"
	dbTypes "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
)

const (
	actionsFileName string = "actions.yaml"
	graphqlFileName        = "actions.graphql"
)

type sdlTo struct {
	SDL map[string]string `json:"sdl"`
}

type sdlFrom struct {
	Types   types.CustomTypes `json:"types"`
	Actions []types.Action    `json:"actions"`
}

type sdlFromDerive struct {
	Derive map[string]interface{} `json:"derive"`
}

type scaffoldResponse struct {
	Files []map[string]string `json:"scaffolds"`
}

type ActionExecutionConfig struct {
	Kind     string                  `json:"default_kind"`
	Handler  string                  `json:"default_handler"`
	Scaffold ScaffoldExecutionConfig `json:"scaffold"`
}

type ScaffoldExecutionConfig struct {
	Default           string            `json:"default,omitempty"`
	OutputDir         string            `json:"output_dir,omitempty"`
	CustomScaffolders map[string]string `json:"custom_scaffolders"`
}

type ActionConfig struct {
	MetadataDir  string
	ActionConfig ActionExecutionConfig
}

func New(baseDir string, actionConfig ActionExecutionConfig) *ActionConfig {
	return &ActionConfig{
		MetadataDir:  baseDir,
		ActionConfig: actionConfig,
	}
}

func (a *ActionConfig) Create(name string, introSchema interface{}, deriveFromMutation string) error {
	graphByt, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, graphqlFileName))
	if err != nil {
		return err
	}
	doc, err := parser.Parse(parser.ParseParams{
		Source: string(graphByt),
	})
	if err != nil {
		return err
	}
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
	""" Define your action as a mutation here """
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
		from := sdlFromDerive{
			Derive: map[string]interface{}{
				"introspection_schema": introSchema,
				"mutation": map[string]string{
					"name":        deriveFromMutation,
					"action_name": name,
				},
			},
		}
		fromByt, err := json.Marshal(from)
		if err != nil {
			return err
		}
		out, err := exec.Command("scaffolder", "sdl", "to", string(fromByt)).Output()
		if err != nil {
			return err
		}
		var to sdlTo
		err = json.Unmarshal(out, &to)
		if err != nil {
			return err
		}
		defaultSDL = to.SDL["complete"]
	}
	newDoc, err := parser.Parse(parser.ParseParams{
		Source: defaultSDL,
	})
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
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, graphqlFileName), data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (a *ActionConfig) Scaffold(name string, scaffolderName string, derivePayload map[string]interface{}) error {
	graphByt, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, graphqlFileName))
	if err != nil {
		return err
	}
	data := map[string]interface{}{
		"action_name": name,
		"framework":   scaffolderName,
		"sdl": map[string]string{
			"complete": string(graphByt),
		},
		"scaffold_config": a.ActionConfig.Scaffold,
		"derive":          derivePayload,
	}
	dataByt, err := json.Marshal(data)
	if err != nil {
		return err
	}
	out, err := exec.Command("scaffolder", "scaffold", string(dataByt)).Output()
	if err != nil {
		return err
	}
	var resp scaffoldResponse
	err = json.Unmarshal(out, &resp)
	if err != nil {
		return err
	}
	for _, file := range resp.Files {
		err = ioutil.WriteFile(filepath.Join(a.ActionConfig.Scaffold.OutputDir, file["name"]), []byte(file["content"]), 0644)
		if err != nil {
			return err
		}
	}
	return nil
}

func (a *ActionConfig) Validate() error {
	return nil
}

func (a *ActionConfig) Build(metadata *dbTypes.Metadata) error {
	graphByt, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, graphqlFileName))
	if err != nil {
		return err
	}
	to := sdlTo{
		SDL: map[string]string{
			"complete": string(graphByt),
		},
	}
	toByt, err := json.Marshal(to)
	if err != nil {
		return err
	}
	out, err := exec.Command("scaffolder", "sdl", "from", string(toByt)).Output()
	if err != nil {
		return err
	}
	var newAction sdlFrom
	err = json.Unmarshal(out, &newAction)
	if err != nil {
		return err
	}
	// Read actions.yaml
	commonByt, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, actionsFileName))
	if err != nil {
		return err
	}
	var oldAction types.Common
	err = gyaml.Unmarshal(commonByt, &oldAction)
	if err != nil {
		return err
	}
	for actionIndex, action := range oldAction.Actions {
		var isFound bool
		for newActionIndex, newActionObj := range newAction.Actions {
			if action.Name == newActionObj.Name {
				isFound = true
				newAction.Actions[newActionIndex].Permissions = oldAction.Actions[actionIndex].Permissions
				newAction.Actions[newActionIndex].Definition.Kind = oldAction.Actions[actionIndex].Definition.Kind
				newAction.Actions[newActionIndex].Definition.Handler = oldAction.Actions[actionIndex].Definition.Handler
				break
			}
		}
		if !isFound {
			return fmt.Errorf("action %s is not present in %s", action.Name, graphqlFileName)
		}
	}
	for customTypeIndex, customType := range oldAction.CustomTypes.Enums {
		var isFound bool
		for newTypeObjIndex, newTypeObj := range newAction.Types.Enums {
			if customType.Name == newTypeObj.Name {
				isFound = true
				newAction.Types.Enums[newTypeObjIndex].Description = oldAction.CustomTypes.Enums[customTypeIndex].Description
				newAction.Types.Enums[newTypeObjIndex].Relationships = oldAction.CustomTypes.Enums[customTypeIndex].Relationships
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)
		}
	}
	for customTypeIndex, customType := range oldAction.CustomTypes.InputObjects {
		var isFound bool
		for newTypeObjIndex, newTypeObj := range newAction.Types.InputObjects {
			if customType.Name == newTypeObj.Name {
				isFound = true
				newAction.Types.InputObjects[newTypeObjIndex].Description = oldAction.CustomTypes.InputObjects[customTypeIndex].Description
				newAction.Types.InputObjects[newTypeObjIndex].Relationships = oldAction.CustomTypes.InputObjects[customTypeIndex].Relationships
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)
		}
	}
	for customTypeIndex, customType := range oldAction.CustomTypes.Objects {
		var isFound bool
		for newTypeObjIndex, newTypeObj := range newAction.Types.Objects {
			if customType.Name == newTypeObj.Name {
				isFound = true
				newAction.Types.Objects[newTypeObjIndex].Description = oldAction.CustomTypes.Objects[customTypeIndex].Description
				newAction.Types.Objects[newTypeObjIndex].Relationships = oldAction.CustomTypes.Objects[customTypeIndex].Relationships
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)
		}
	}
	for customTypeIndex, customType := range oldAction.CustomTypes.Scalars {
		var isFound bool
		for newTypeObjIndex, newTypeObj := range newAction.Types.Scalars {
			if customType.Name == newTypeObj.Name {
				isFound = true
				newAction.Types.Scalars[newTypeObjIndex].Description = oldAction.CustomTypes.Scalars[customTypeIndex].Description
				newAction.Types.Scalars[newTypeObjIndex].Relationships = oldAction.CustomTypes.Scalars[customTypeIndex].Relationships
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, graphqlFileName)
		}
	}
	for index, action := range newAction.Actions {
		if action.Definition.Kind == "" {
			newAction.Actions[index].Definition.Kind = a.ActionConfig.Kind
		}
		if action.Definition.Handler == "" {
			newAction.Actions[index].Definition.Handler = a.ActionConfig.Handler + "/" + action.Name
		}
	}
	var common types.Common
	common.Actions = newAction.Actions
	common.CustomTypes = newAction.Types
	// write actions.yaml and custom_types.yaml
	commonByt, err = yaml.Marshal(common)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, actionsFileName), commonByt, 0644)
	if err != nil {
		return err
	}
	metadata.Actions = common.Actions
	metadata.CustomTypes = common.CustomTypes
	return nil
}

func (a *ActionConfig) Export(metadata dbTypes.Metadata) error {
	tmpByt, err := json.Marshal(metadata)
	if err != nil {
		return err
	}
	var common types.Common
	err = json.Unmarshal(tmpByt, &common)
	if err != nil {
		return err
	}
	var dataFrom sdlFrom
	dataFrom.Types = common.CustomTypes
	dataFrom.Actions = common.Actions
	fromByt, err := json.Marshal(dataFrom)
	if err != nil {
		return err
	}
	out, err := exec.Command("scaffolder", "sdl", "to", string(fromByt)).Output()
	if err != nil {
		return err
	}
	var data sdlTo
	err = json.Unmarshal(out, &data)
	if err != nil {
		return err
	}
	doc, err := parser.Parse(parser.ParseParams{
		Source: data.SDL["complete"],
	})
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
