package commands

import (
	"fmt"
	"os"

	"github.com/hasura/graphql-engine/cli"
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
		Short:        "Generate auto completion code",
		Args:         cobra.ExactArgs(1),
		Long:         "Output shell completion code for the specified shell (bash or zsh)",
		SilenceUsage: true,
		Example:      completionCmdExample,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = viper.New()
			return ec.Prepare()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.Shell = args[0]
			opts.Cmd = cmd
			return opts.run()
		},
	}

	completionCmd.Flags().StringVar(&opts.File, "file", "", "file to which output has to be written")
	completionCmd.MarkFlagFilename("file")
	return completionCmd
}

type completionOptions struct {
	EC *cli.ExecutionContext

	Shell string
	File  string
	Cmd   *cobra.Command
}

func (o *completionOptions) run() error {
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
		err = fmt.Errorf("Unknown shell: %s. Use bash or zsh", o.Shell)
	}
	if err != nil {
		return err
	}
	return nil
}
