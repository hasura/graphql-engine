package commands

import (
	"fmt"
	"path/filepath"
	"sort"
	"strconv"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/util"

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
		Short: "Use the codegen to generate code for Hasura Actions",
		Long: `This command generates code for Hasura Actions using the codegen framework of your choice. While not required, you can pass the ` + "``--framework``" + ` flag to select a framework. If you do not pass the ` + "``--framework``" + ` flag, you will be prompted to select a framework from a list of available options.

Further Reading:
- https://hasura.io/docs/latest/actions/codegen/index/#codegen-for-your-framework
`,
		Example: `  # Use codegen by providing framework
  hasura actions use-codegen --framework nodejs-express

  # Use codegen from framework list
  hasura actions use-codegen

  # Set output directory
  hasura actions use-codegen --output-dir codegen

  # Use a codegen with a starter kit
  hasura actions use-codegen --with-starter-kit true`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			if err := ec.SetupCodegenAssetsRepo(); err != nil {
				return errors.E(op, fmt.Errorf("setting up codegen-assets repo failed (this is required for automatically generating actions code): %w", err))
			}

			if err := ec.SetupCodegenAssetsRepo(); err != nil {
				return errors.E(op, "setting up codengen assets repo failed")
			}
			// ensure codegen-assets repo exists
			if err := ec.CodegenAssetsRepo.EnsureCloned(); err != nil {
				return errors.E(op, fmt.Errorf("pulling latest actions codegen files from internet failed: %w", err))
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			if err := opts.run(); err != nil {
				return errors.E(op, err)
			}
			return nil
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
	var op errors.Op = "commands.actionsUseCodegenOptions.run"
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
		return errors.E(op, fmt.Errorf("error in fetching codegen frameworks: %w", err))
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
			return errors.E(op, fmt.Errorf("error in selecting framework: %w", err))
		}
	} else {
		for _, f := range allFrameworks {
			if o.framework == f.Name {
				newCodegenExecutionConfig.Framework = o.framework
			}
		}
		if newCodegenExecutionConfig.Framework == "" {
			return errors.E(op, fmt.Errorf("framework %s is not found", o.framework))
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
		return errors.E(op, fmt.Errorf("starter kit is not available for framework %s", newCodegenExecutionConfig.Framework))
	}

	// if with-starter-kit flag is not provided, give an option to clone a starterkit
	if !o.withStarterKit && hasStarterKit {
		shouldCloneStarterKit, err := util.GetYesNoPrompt("Do you also want to clone a starter kit for " + newCodegenExecutionConfig.Framework + "?")
		if err != nil {
			return errors.E(op, fmt.Errorf("error in getting input from user: %w", err))
		}
		o.withStarterKit = shouldCloneStarterKit
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

		// copy the starter kit
		destinationDir := filepath.Join(o.EC.ExecutionDirectory, starterKitDirname)
		err = util.FSCopyDir(
			filepath.Join(o.EC.GlobalConfigDir, util.ActionsCodegenDirName, newCodegenExecutionConfig.Framework, "starter-kit"),
			destinationDir,
		)
		if err != nil {
			return errors.E(op, fmt.Errorf("error in copying starter kit: %w", err))
		}
		o.EC.Logger.Info("Starter kit cloned at " + destinationDir)
	}
	o.EC.Spinner.Stop()

	// if output directory is not provided, make them enter it
	if o.outputDir == "" {
		outputDir, err := util.GetFSPathPrompt("Where do you want to place the codegen files?", o.EC.Config.ActionConfig.Codegen.OutputDir)
		if err != nil {
			return errors.E(op, fmt.Errorf("error in getting output directory input: %w", err))
		}
		newCodegenExecutionConfig.OutputDir = outputDir
	} else {
		newCodegenExecutionConfig.OutputDir = o.outputDir
	}

	newConfig := o.EC.Config
	newConfig.ActionConfig.Codegen = newCodegenExecutionConfig
	err = o.EC.WriteConfig(newConfig)
	if err != nil {
		return errors.E(op, fmt.Errorf("error in writing config: %w", err))
	}
	o.EC.Spinner.Stop()
	o.EC.Logger.Info("Codegen configuration updated in config.yaml")
	return nil
}
