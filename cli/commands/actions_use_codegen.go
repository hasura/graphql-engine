package commands

import (
	"fmt"
	"path/filepath"
	"sort"
	"strconv"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

type codegenFramework struct {
	Name          string `json:"name"`
	HasStarterKit bool   `json:"hasStarterKit"`
}

func newActionsUseCodegenCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &actionsUseCodegenOptions{
		EC: ec,
	}
	actionsUseCodegenCmd := &cobra.Command{
		Use:   "use-codegen",
		Short: "Configure the codegen module",
		Example: `  # Use codegen by providing framework
  hasura actions use-codegen --framework nodejs-express

  # Use codegen from framework list
  hasura actions use-codegen

  # Set output directory
  hasura actions use-codegen --output-dir codegen

  # Use a codegen with a starter kit
  hasura actions use-codegen --with-starter-kit true`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}

	f := actionsUseCodegenCmd.Flags()

	f.StringVar(&opts.framework, "framework", "", "framework to be used by codegen")
	f.StringVar(&opts.outputDir, "output-dir", "", "directory to create the codegen files")
	f.BoolVar(&opts.withStarterKit, "with-starter-kit", false, "clone starter kit for a framework")

	return actionsUseCodegenCmd
}

type actionsUseCodegenOptions struct {
	EC *cli.ExecutionContext

	framework      string
	outputDir      string
	withStarterKit bool
}

func (o *actionsUseCodegenOptions) run() error {
	o.EC.Spin("Ensuring codegen-assets repo is updated...")
	defer o.EC.Spinner.Stop()
	// ensure the the actions-codegen repo is updated
	err := o.EC.CodegenAssetsRepo.EnsureUpdated()
	if err != nil {
		o.EC.Logger.Warnf("unable to update codegen-assets repo, got %v", err)
	}

	newCodegenExecutionConfig := o.EC.Config.ActionConfig.Codegen
	newCodegenExecutionConfig.Framework = ""

	o.EC.Spin("Fetching frameworks...")
	allFrameworks, err := getCodegenFrameworks()
	if err != nil {
		return errors.Wrap(err, "error in fetching codegen frameworks")
	}

	o.EC.Spinner.Stop()
	if o.framework == "" {
		// if framework flag is not provided, display a list and allow them to choose
		var frameworkList []string
		for _, f := range allFrameworks {
			frameworkList = append(frameworkList, f.Name)
		}
		sort.Strings(frameworkList)
		newCodegenExecutionConfig.Framework, err = util.GetSelectPrompt("Choose a codegen framework to use", frameworkList)
		if err != nil {
			return errors.Wrap(err, "error in selecting framework")
		}
	} else {
		for _, f := range allFrameworks {
			if o.framework == f.Name {
				newCodegenExecutionConfig.Framework = o.framework
			}
		}
		if newCodegenExecutionConfig.Framework == "" {
			return fmt.Errorf("framework %s is not found", o.framework)
		}
	}

	hasStarterKit := false
	for _, f := range allFrameworks {
		if f.Name == newCodegenExecutionConfig.Framework && f.HasStarterKit {
			hasStarterKit = true
		}
	}

	// if with-starter-kit flag is set and the same is not available for the framework, return error
	if o.withStarterKit && !hasStarterKit {
		return fmt.Errorf("starter kit is not available for framework %s", newCodegenExecutionConfig.Framework)
	}

	// if with-starter-kit flag is not provided, give an option to clone a starterkit
	if !o.withStarterKit && hasStarterKit {
		shouldCloneStarterKit, err := util.GetYesNoPrompt("Do you also want to clone a starter kit for " + newCodegenExecutionConfig.Framework + "?")
		if err != nil {
			return errors.Wrap(err, "error in getting input from user")
		}
		o.withStarterKit = shouldCloneStarterKit == "y"
	}

	// if output directory is not provided, make them enter it
	if o.outputDir == "" {
		outputDir, err := util.GetFSPathPrompt("Where do you want to place the codegen files?", o.EC.Config.ActionConfig.Codegen.OutputDir)
		if err != nil {
			return errors.Wrap(err, "error in getting output directory input")
		}
		newCodegenExecutionConfig.OutputDir = outputDir
	} else {
		newCodegenExecutionConfig.OutputDir = o.outputDir
	}

	// clone the starter kit
	o.EC.Spin("Clonning the starter kit...")
	if o.withStarterKit && hasStarterKit {
		// get a directory name to clone the starter kit in
		starterKitDirname := newCodegenExecutionConfig.Framework
		err = util.FSCheckIfDirPathExists(
			filepath.Join(o.EC.ExecutionDirectory, starterKitDirname),
		)
		suffix := 2
		for err == nil {
			starterKitDirname = newCodegenExecutionConfig.Framework + "-" + strconv.Itoa(suffix)
			suffix++
			err = util.FSCheckIfDirPathExists(starterKitDirname)
		}
		err = nil

		// copy the starter kit
		destinationDir := filepath.Join(o.EC.ExecutionDirectory, starterKitDirname)
		err = util.FSCopyDir(
			filepath.Join(o.EC.GlobalConfigDir, util.ActionsCodegenDirName, newCodegenExecutionConfig.Framework, "starter-kit"),
			destinationDir,
		)
		if err != nil {
			return errors.Wrap(err, "error in copying starter kit")
		}
		o.EC.Logger.Info("Starter kit cloned at " + destinationDir)
	}

	newConfig := o.EC.Config
	newConfig.ActionConfig.Codegen = newCodegenExecutionConfig
	err = o.EC.WriteConfig(newConfig)
	if err != nil {
		return errors.Wrap(err, "error in writing config")
	}
	o.EC.Spinner.Stop()
	o.EC.Logger.Info("Codegen configuration updated in config.yaml")
	return nil
}
