package commands

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/aryann/difflib"
	"github.com/hasura/graphql-engine/cli"
	"github.com/mgutz/ansi"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	v2yaml "gopkg.in/yaml.v2"
)

type metadataDiffOptions struct {
	EC     *cli.ExecutionContext
	output io.Writer

	// two metadata to diff, 2nd is server if it's empty
	metadata [2]string
}

func newMetadataDiffCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &metadataDiffOptions{
		EC:     ec,
		output: os.Stdout,
	}

	metadataDiffCmd := &cobra.Command{
		Use:   "diff [file1] [file2]",
		Short: "(PREVIEW) Show a highlighted diff of Hasura metadata",
		Long: `(PREVIEW) Show changes between two different sets of Hasura metadata.
By default, shows changes between exported metadata file and server metadata.`,
		Example: `  # NOTE: This command is in preview, usage and diff format may change.

  # Show changes between server metadata and the exported metadata file:
  hasura metadata diff

  # Show changes between server metadata and that in local_metadata.yaml:
  hasura metadata diff local_metadata.yaml

  # Show changes between metadata from metadata.yaml and metadata_old.yaml:
  hasura metadata diff metadata.yaml metadata_old.yaml

  # Apply admin secret for Hasura GraphQL Engine:
  hasura metadata diff --admin-secret "<admin-secret>"

  # Diff metadata on a different Hasura instance:
  hasura metadata diff --endpoint "<endpoint>"`,
		Args: cobra.MaximumNArgs(2),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			messageFormat := "Showing diff between %s and %s..."
			message := ""

			switch len(args) {
			case 0:
				// no args, diff exported metadata and metadata on server
				filename, err := ec.GetExistingMetadataFile()
				if err != nil {
					return errors.Wrap(err, "failed getting metadata file")
				}
				opts.metadata[0] = filename
				message = fmt.Sprintf(messageFormat, filename, "the server")
			case 1:
				// 1 arg, diff given filename and the metadata on server
				opts.metadata[0] = args[0]
				message = fmt.Sprintf(messageFormat, args[0], "the server")
			case 2:
				// 2 args, diff given filenames
				opts.metadata[0] = args[0]
				opts.metadata[1] = args[1]
				message = fmt.Sprintf(messageFormat, args[0], args[1])
			}

			opts.EC.Logger.Info(message)
			err := opts.run()
			if err != nil {
				return errors.Wrap(err, "failed to show metadata diff")
			}
			return nil
		},
	}

	f := metadataDiffCmd.Flags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))

	return metadataDiffCmd
}

func (o *metadataDiffOptions) run() error {
	var oldYaml, newYaml []byte
	var err error
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version, true)
	if err != nil {
		return err
	}

	if o.metadata[1] == "" {
		// get metadata from server
		m, err := migrateDrv.ExportMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot fetch metadata from server")
		}

		newYaml, err = v2yaml.Marshal(m)
		if err != nil {
			return errors.Wrap(err, "cannot convert metadata from server to yaml")
		}
	} else {
		newYaml, err = ioutil.ReadFile(o.metadata[1])
		if err != nil {
			return errors.Wrap(err, "cannot read file")
		}
	}

	oldYaml, err = ioutil.ReadFile(o.metadata[0])
	if err != nil {
		return errors.Wrap(err, "cannot read file")
	}

	printDiff(string(oldYaml), string(newYaml), o.output)
	return nil
}

func printDiff(before, after string, to io.Writer) {
	diffs := difflib.Diff(strings.Split(before, "\n"), strings.Split(after, "\n"))

	for _, diff := range diffs {
		text := diff.Payload

		switch diff.Delta {
		case difflib.RightOnly:
			fmt.Fprintf(to, "%s\n", ansi.Color(text, "green"))
		case difflib.LeftOnly:
			fmt.Fprintf(to, "%s\n", ansi.Color(text, "red"))
		case difflib.Common:
			fmt.Fprintf(to, "%s\n", text)
		}
	}
}
