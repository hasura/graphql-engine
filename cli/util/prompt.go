package util

import (
	"github.com/manifoldco/promptui"
)

func GetYesNoPrompt(message string) (cgPromptResp string, err error) {

	cgPrompt := promptui.Prompt{
		Label: message,
		IsConfirm: true,
	}

	cgPromptResp, err = cgPrompt.Run()
	if err != nil {
		return
	}

	return

}