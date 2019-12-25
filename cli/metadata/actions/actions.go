package actions

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os/exec"
	"path/filepath"

	"github.com/graphql-go/graphql/language/kinds"
	"github.com/graphql-go/graphql/language/parser"
	"github.com/graphql-go/graphql/language/printer"

	gyaml "github.com/ghodss/yaml"
	"gopkg.in/yaml.v2"

	"github.com/graphql-go/graphql/language/ast"
	"github.com/hasura/graphql-engine/cli/metadata/actions/editor"
	"github.com/hasura/graphql-engine/cli/metadata/actions/types"
	dbTypes "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
)

const (
	actionsFileName     string = "actions.yaml"
	graphqlFileName            = "actions.graphql"
	customTypesFileName        = "custom_types.yaml"
)

type sdlTo struct {
	SDL map[string]string `json:"sdl"`
}

type sdlFrom struct {
	Types   types.CustomTypes `json:"types"`
	Actions []types.Action    `json:"actions"`
}

type ActionConfig struct {
	MetadataDir string
}

func New(baseDir string) *ActionConfig {
	return &ActionConfig{
		MetadataDir: baseDir,
	}
}

func (a *ActionConfig) Create(name string) error {
	// Parse the actions.graphql
	graphByt, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, graphqlFileName))
	if err != nil {
		return err
	}
	doc, err := parser.Parse(parser.ParseParams{
		Source: string(graphByt),
		Options: parser.ParseOptions{
			NoLocation: true,
		},
	})
	if err != nil {
		return err
	}
	newDefaultMutation := &ast.ObjectDefinition{
		Kind: kinds.ObjectDefinition,
		Name: &ast.Name{
			Kind:  kinds.Name,
			Value: "Mutation",
		},
		Fields: make([]*ast.FieldDefinition, 0),
	}
	newDefaultField := &ast.FieldDefinition{
		Kind: kinds.FieldDefinition,
		Description: &ast.StringValue{
			Kind:  kinds.StringValue,
			Value: "Define your action as a mutation here",
		},
		Name: &ast.Name{
			Kind:  kinds.Name,
			Value: name,
		},
		Type: &ast.Named{
			Kind: kinds.Named,
			Name: &ast.Name{
				Kind:  kinds.Name,
				Value: "SampleOutput",
			},
		},
		Arguments: make([]*ast.InputValueDefinition, 0),
	}
	newDefaultArg := &ast.InputValueDefinition{
		Kind: kinds.InputValueDefinition,
		Name: &ast.Name{
			Kind:  kinds.Name,
			Value: "arg1",
		},
		Type: &ast.NonNull{
			Kind: kinds.NonNull,
			Type: &ast.Named{
				Kind: kinds.Named,
				Name: &ast.Name{
					Kind:  kinds.Name,
					Value: "SampleInput",
				},
			},
		},
	}
	newDefaultField.Arguments = append(newDefaultField.Arguments, newDefaultArg)
	newDefaultMutation.Fields = append(newDefaultMutation.Fields, newDefaultField)
	doc.Definitions = append([]ast.Node{newDefaultMutation}, doc.Definitions...)
	for index, def := range doc.Definitions {
		if index == 0 {
			continue
		}
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
	defaultText := printer.Print(doc).(string)
	data, err := editor.CaptureInputFromEditor(editor.GetPreferredEditorFromEnvironment, defaultText)
	if err != nil {
		return err
	}
	fmt.Println(string(data))
	return nil
}

func (a *ActionConfig) Validate() error {
	return nil
}

func (a *ActionConfig) Build(metadata *dbTypes.Metadata) error {
	graphByt, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, graphqlFileName))
	if err != nil {
		fmt.Println("1")
		return err
	}
	to := sdlTo{
		SDL: map[string]string{
			"complete": string(graphByt),
		},
	}
	toByt, err := json.Marshal(to)
	if err != nil {
		fmt.Println("2")
		return err
	}
	out, err := exec.Command("scaffolder", "sdl", "from", string(toByt)).Output()
	if err != nil {
		fmt.Println("3")
		return err
	}
	var newAction sdlFrom
	err = json.Unmarshal(out, &newAction)
	if err != nil {
		fmt.Println("4")
		return err
	}
	// Read actions.yaml and custom_types.yaml
	actionByt, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, actionsFileName))
	if err != nil {
		fmt.Println("5")
		return err
	}
	cusTypeByt, err := ioutil.ReadFile(filepath.Join(a.MetadataDir, customTypesFileName))
	if err != nil {
		fmt.Println("6")
		return err
	}
	var oldAction sdlFrom
	err = gyaml.Unmarshal(cusTypeByt, &oldAction.Types)
	if err != nil {
		fmt.Println("8")
		return err
	}
	err = gyaml.Unmarshal(actionByt, &oldAction.Actions)
	if err != nil {
		fmt.Println("7")
		return err
	}
	for actionIndex, action := range oldAction.Actions {
		var isFound bool
		for newActionIndex, newActionObj := range newAction.Actions {
			if action.Name == newActionObj.Name {
				isFound = true
				newAction.Actions[newActionIndex].Permissions = oldAction.Actions[actionIndex].Permissions
				newAction.Actions[newActionIndex].Definition.Kind = oldAction.Actions[actionIndex].Definition.Kind
				newAction.Actions[newActionIndex].Definition.Webhook = oldAction.Actions[actionIndex].Definition.Webhook
				break
			}
		}
		if !isFound {
			return fmt.Errorf("action %s is not present in %s", action.Name, graphqlFileName)
		}
	}
	for customTypeIndex, customType := range oldAction.Types.Enums {
		var isFound bool
		for newTypeObjIndex, newTypeObj := range newAction.Types.Enums {
			if customType.Name == newTypeObj.Name {
				isFound = true
				newAction.Types.Enums[newTypeObjIndex].Description = oldAction.Types.Enums[customTypeIndex].Description
				newAction.Types.Enums[newTypeObjIndex].Relationships = oldAction.Types.Enums[customTypeIndex].Relationships
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, customTypesFileName)
		}
	}
	for customTypeIndex, customType := range oldAction.Types.InputObjects {
		var isFound bool
		for newTypeObjIndex, newTypeObj := range newAction.Types.InputObjects {
			if customType.Name == newTypeObj.Name {
				isFound = true
				fmt.Println(newAction.Types.InputObjects[newTypeObjIndex].Fields)
				newAction.Types.InputObjects[newTypeObjIndex].Description = oldAction.Types.InputObjects[customTypeIndex].Description
				newAction.Types.InputObjects[newTypeObjIndex].Relationships = oldAction.Types.InputObjects[customTypeIndex].Relationships
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, customTypesFileName)
		}
	}
	for customTypeIndex, customType := range oldAction.Types.Objects {
		var isFound bool
		for newTypeObjIndex, newTypeObj := range newAction.Types.Objects {
			if customType.Name == newTypeObj.Name {
				isFound = true
				newAction.Types.Objects[newTypeObjIndex].Description = oldAction.Types.Objects[customTypeIndex].Description
				newAction.Types.Objects[newTypeObjIndex].Relationships = oldAction.Types.Objects[customTypeIndex].Relationships
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, customTypesFileName)
		}
	}
	for customTypeIndex, customType := range oldAction.Types.Scalars {
		var isFound bool
		for newTypeObjIndex, newTypeObj := range newAction.Types.Scalars {
			if customType.Name == newTypeObj.Name {
				isFound = true
				newAction.Types.Scalars[newTypeObjIndex].Description = oldAction.Types.Scalars[customTypeIndex].Description
				newAction.Types.Scalars[newTypeObjIndex].Relationships = oldAction.Types.Scalars[customTypeIndex].Relationships
				break
			}
		}
		if !isFound {
			return fmt.Errorf("custom type %s is not present in %s", customType.Name, customTypesFileName)
		}
	}
	metadata.Actions = newAction.Actions
	metadata.CustomTypes = newAction.Types
	return nil
}

func (a *ActionConfig) Export(metadata dbTypes.Metadata) error {
	var dataFrom sdlFrom
	dataFrom.Types = metadata.CustomTypes
	dataFrom.Actions = metadata.Actions
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
	actionByt, err := yaml.Marshal(metadata.Actions)
	if err != nil {
		return err
	}
	customTypesByt, err := yaml.Marshal(metadata.CustomTypes)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, actionsFileName), actionByt, 0644)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, customTypesFileName), customTypesByt, 0644)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filepath.Join(a.MetadataDir, graphqlFileName), []byte(data.SDL["complete"]), 0644)
	if err != nil {
		return err
	}
	return nil
}
