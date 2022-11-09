package util

import (
	"github.com/AlecAivazis/survey/v2"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

func GetYesNoPrompt(message string) (promptResp bool, err error) {
	var op errors.Op = "util.GetYesNoPrompt"
	prompt := &survey.Confirm{
		Message: message,
		Default: true,
	}
	err = survey.AskOne(prompt, &promptResp)
	if err != nil {
		return promptResp, errors.E(op, err)
	}
	return promptResp, nil
}

func GetSelectPrompt(message string, options []string) (selection string, err error) {
	var op errors.Op = "util.GetSelectPrompt"
	prompt := &survey.Select{
		Message: message,
		Options: options,
	}
	err = survey.AskOne(prompt, &selection)
	if err != nil {
		return selection, errors.E(op, err)
	}
	return selection, nil
}

func GetInputPrompt(message string) (input string, err error) {
	var op errors.Op = "util.GetInputPrompt"
	prompt := &survey.Input{
		Message: message,
	}
	err = survey.AskOne(prompt, &input)
	if err != nil {
		return input, errors.E(op, err)
	}
	return input, nil
}

func GetInputPromptWithDefault(message string, def string) (input string, err error) {
	var op errors.Op = "util.GetInputPromptWithDefault"
	prompt := &survey.Input{
		Message: message,
		Default: def,
	}
	err = survey.AskOne(prompt, &input)
	if err != nil {
		return input, errors.E(op, err)
	}
	return input, nil
}

func validateDirPath(a interface{}) error {
	var op errors.Op = "util.validateDirPath"
	err := FSCheckIfDirPathExists(a.(string))
	if err != nil {
		return errors.E(op, err) 
	}
	return nil
}

func GetFSPathPrompt(message string, def string) (input string, err error) {
	var op errors.Op = "util.GetFSPathPrompt"
	prompt := &survey.Input{
		Message: message,
		Default: def,
	}
	err = survey.AskOne(prompt, &input, survey.WithValidator(validateDirPath))
	if err != nil {
		return input, errors.E(op, err) 
	}
	return input, nil
}
