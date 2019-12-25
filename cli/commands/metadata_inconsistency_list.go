package commands

import (
	"bytes"
	"fmt"
	"text/tabwriter"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate/database"
	"github.com/hasura/graphql-engine/cli/util"
)

func newMetadataInconsistencyListCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &metadataInconsistencyListOptions{
		EC: ec,
	}

	metadataInconsistencyListCmd := &cobra.Command{
		Use:          "list",
		Aliases:      []string{"ls"},
		Short:        "List all inconsistent objects from the metadata",
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			err := opts.run()
			opts.EC.Spinner.Stop()
			if err != nil {
				return errors.Wrap(err, "failed to list inconsistent metadata")
			}
			if opts.isConsistent {
				opts.EC.Logger.Println("metadata is consistent")
			}
			return nil
		},
	}

	f := metadataInconsistencyListCmd.Flags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return metadataInconsistencyListCmd
}

type metadataInconsistencyListOptions struct {
	EC *cli.ExecutionContext

	isConsistent        bool
	inconsistentObjects []database.InconsistentMetadataInterface
}

func (o *metadataInconsistencyListOptions) read() error {
	d, err := newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version, true)
	if err != nil {
		return err
	}
	o.isConsistent, o.inconsistentObjects, err = d.GetInconsistentMetadata()
	if err != nil {
		return err
	}
	return nil
}

func (o *metadataInconsistencyListOptions) run() error {
	o.EC.Spin("Getting inconsistent metadata...")

	err := o.read()
	if err != nil {
		return err
	}
	if o.isConsistent {
		return nil
	}
	out := new(tabwriter.Writer)
	buf := &bytes.Buffer{}
	out.Init(buf, 0, 8, 2, ' ', 0)
	w := util.NewPrefixWriter(out)
	w.Write(util.LEVEL_0, "NAME\tTYPE\tDESCRIPTION\tREASON\n")
	for _, obj := range o.inconsistentObjects {
		w.Write(util.LEVEL_0, "%s\t%s\t%s\t%s\n",
			obj.GetName(),
			obj.GetType(),
			obj.GetDescription(),
			obj.GetReason(),
		)
	}
	out.Flush()
	o.EC.Spinner.Stop()
	fmt.Println(buf.String())
	return nil
}
