package commands

import (
	"fmt"
	"os"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

const completionCmdExample = `# Bash
    # Linux
      # Add Bash completion file using:
      $ sudo hasura completion bash --file=/etc/bash_completion.d/hasura
    # Mac
      # Install bash-completion using homebrew:
      $ brew install bash-completion
      # Add to your ~/.bash_profile:
      if [ -f $(brew --prefix)/etc/bash_completion ]; then
          . $(brew --prefix)/etc/bash_completion
      fi
      # Add the completion file:
      $ sudo hasura completion bash --file=$(brew --prefix)/etc/bash_completion.d/hasura
    # Windows (Git Bash)
      # open git bash
      $ mkdir -p ~/.bash_completion.d
      # Add the completion file:
      $ cd ~ && hasura completion bash --file=.bash_completion.d/hasura
      # Add the following to ~/.bash_profile
        if [ -f ~/.bash_completion.d/hasura ]; then
          . ~/.bash_completion.d/hasura
        fi
      # restart git bash

  # Zsh (using oh-my-zsh)
    $ mkdir -p $HOME/.oh-my-zsh/completions
    $ hasura completion zsh --file=$HOME/.oh-my-zsh/completions/_hasura

  # Reload the shell for the changes to take effect!`

// NewCompletionCmd return the completion command.
func NewCompletionCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &completionOptions{
		EC: ec,
	}
	completionCmd := &cobra.Command{
		Use:          "completion [shell]",
		Short:        "Generate auto-completion code",
		Args:         cobra.ExactArgs(1),
		Long:         "Depending on your shell (bash or zsh), running `hasura completion [shell]` will generate the auto-completion code for the Hasura CLI. You can then add this to your shell config to enable auto-completion, which will allow you to tab through the available commands and options.",
		SilenceUsage: true,
		Example:      completionCmdExample,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			ec.Viper = viper.New()
			if err := ec.Prepare(); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			opts.Shell = args[0]
			opts.Cmd = cmd
			if err := opts.run(); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
	}

	completionCmd.Flags().StringVar(&opts.File, "file", "", "file to which output has to be written")
	return completionCmd
}

type completionOptions struct {
	EC *cli.ExecutionContext

	Shell string
	File  string
	Cmd   *cobra.Command
}

func (o *completionOptions) run() error {
	var op errors.Op = "commands.completionOptions.run"
	var err error
	switch o.Shell {
	case "bash":
		if o.File != "" {
			err = o.Cmd.Root().GenBashCompletionFile(o.File)
		} else {
			err = o.Cmd.Root().GenBashCompletion(os.Stdout)
		}
	case "zsh":
		if o.File != "" {
			err = o.Cmd.Root().GenZshCompletionFile(o.File)
		} else {
			err = o.Cmd.Root().GenZshCompletion(os.Stdout)
		}
	default:
		err = fmt.Errorf("unknown shell: %s. Use bash or zsh", o.Shell)
	}
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}
