package commands

import (
	"encoding/json"
	"fmt"

	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"

	"github.com/spf13/cobra"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/util"
)

func newMetadataInconsistencyListCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &metadataInconsistencyListOptions{
		EC: ec,
	}

	metadataInconsistencyListCmd := &cobra.Command{
		Use:          "list",
		Aliases:      []string{"ls"},
		Short:        "List all inconsistent objects from the metadata",
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return fmt.Errorf("failed to list inconsistent metadata: %w", err)
			}
			if opts.isConsistent {
				opts.EC.Logger.Println("metadata is consistent")
			}
			return nil
		},
	}
	f := metadataInconsistencyListCmd.Flags()
	f.StringVarP(&opts.outputFormat, "output", "o", "", "select output format for inconsistent metadata objects(Allowed values: json)")

	return metadataInconsistencyListCmd
}

type metadataInconsistencyListOptions struct {
	EC *cli.ExecutionContext

	outputFormat        string
	isConsistent        bool
	inconsistentObjects []projectmetadata.InconsistentMetadataObject
}

func (o *metadataInconsistencyListOptions) read(handler *projectmetadata.Handler) error {
	var err error
	o.isConsistent, o.inconsistentObjects, err = handler.GetInconsistentMetadata()
	if err != nil {
		return err
	}
	return nil
}

func (o *metadataInconsistencyListOptions) run() error {
	o.EC.Spin("Getting inconsistent metadata...")

	err := o.read(projectmetadata.NewHandlerFromEC(o.EC))
	if err != nil {
		return err
	}
	if o.isConsistent {
		return nil
	}
	if o.outputFormat == "json" {
		jsonBytes, err := json.MarshalIndent(o.inconsistentObjects, "", "  ")
		if err != nil {
			return err
		}
		o.EC.Spinner.Stop()
		fmt.Fprintln(o.EC.Stdout, string(jsonBytes))
		return nil
	}
	table := util.NewTableWriter(o.EC.Stdout)
	table.SetHeader([]string{"NAME", "TYPE", "DESCRIPTION", "REASON"})
	for _, obj := range o.inconsistentObjects {
		table.Append([]string{
			obj.GetName(),
			obj.GetType(),
			obj.GetDescription(),
			obj.GetReason(),
		})
	}
	o.EC.Spinner.Stop()
	table.Render()
	return nil
}
