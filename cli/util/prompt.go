package util

import (
	"errors"
	"strings"

	"github.com/manifoldco/promptui"
)

const (
	INVALID_YES_NO_RESP_ERROR = "invalid response, please enter Y or N"
)

func GetYesNoPrompt(message string) (promptResp string, err error) {
	prompt := promptui.Prompt{
		Label: message + " (y/n)",
		Validate: func(_resp string) (err error) {
			if len(_resp) == 0 {
				err = errors.New(INVALID_YES_NO_RESP_ERROR)
				return
			}
			resp := strings.ToLower(_resp)
			if resp != "n" && resp != "y" && resp != "no" && resp != "yes" {
				err = errors.New(INVALID_YES_NO_RESP_ERROR)
			}
			return err
		},
		Default: "y",
	}
	promptResp, err = prompt.Run()
	if err != nil {
		return
	}
	promptResp = string(strings.ToLower(promptResp)[0])
	return
}

func GetSelectPrompt(message string, options []string) (selection string, err error) {
	prompt := promptui.Select{
		Label: message,
		Items: options,
	}
	_, selection, err = prompt.Run()
	return
}

func GetFSPathPrompt(message string, def string) (input string, err error) {
	prompt := promptui.Prompt{
		Label:    message,
		Validate: FSCheckIfDirPathExists,
		Default:  def,
	}
	return prompt.Run()
}
