package util

import (
	"github.com/AlecAivazis/survey/v2"
)

func GetYesNoPrompt(message string) (promptResp bool, err error) {
	prompt := &survey.Confirm{
		Message: message,
		Default: true,
	}
	err = survey.AskOne(prompt, &promptResp)
	return promptResp, err
}

func GetSelectPrompt(message string, options []string) (selection string, err error) {
	prompt := &survey.Select{
		Message: message,
		Options: options,
	}
	err = survey.AskOne(prompt, &selection)
	return selection, err
}

func GetInputPrompt(message string) (input string, err error) {
	prompt := &survey.Input{
		Message: message,
	}
	err = survey.AskOne(prompt, &input)
	return input, err
}

func GetInputPromptWithDefault(message string, def string) (input string, err error) {
	prompt := &survey.Input{
		Message: message,
		Default: def,
	}
	err = survey.AskOne(prompt, &input)
	return input, err
}

func validateDirPath(a interface{}) error {
	err := FSCheckIfDirPathExists(a.(string))
	return err
}

func GetFSPathPrompt(message string, def string) (input string, err error) {
	prompt := &survey.Input{
		Message: message,
		Default: def,
	}
	err = survey.AskOne(prompt, &input, survey.WithValidator(validateDirPath))
	return input, err
}
