package commands

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"reflect"
	"strings"

	"github.com/aryann/difflib"
	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	"github.com/mgutz/ansi"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
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
  hasura metadata diff metadata.yaml metadata_old.yaml`,
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

		newYaml, err = yaml.Marshal(m)
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

	// return diffYaml(oldYaml, newYaml, o.output)
	printDiff(string(oldYaml), string(newYaml), o.output)
	return nil
}

func yamlToMap(y []byte) (map[string]string, error) {
	var obj map[string]interface{}
	m := make(map[string]string)
	err := yaml.Unmarshal(y, &obj)
	if err != nil {
		return nil, err
	}
	for key, value := range obj {
		val, err := yaml.Marshal(value)
		if err != nil {
			return m, err
		}
		m[key] = string(val)
	}
	return m, nil
}

func diffYaml(oldYaml, newYaml []byte, to io.Writer) error {
	oldIndex, err := yamlToMap(oldYaml)
	if err != nil {
		return err
	}
	newIndex, err := yamlToMap(newYaml)
	if err != nil {
		return err
	}
	if reflect.DeepEqual(oldIndex, newIndex) {
		fmt.Fprintf(to, "There are no changes.")
		return nil
	}
	for key, oldContent := range oldIndex {
		if newContent, ok := newIndex[key]; ok {
			if oldContent != newContent {
				// modified
				fmt.Fprintf(to, ansi.Color("%s has changed:", "yellow")+"\n", key)
				printDiff(oldContent, newContent, to)
			}
		} else {
			// removed
			fmt.Fprintf(to, ansi.Color("%s has been removed:", "yellow")+"\n", key)
			printDiff(oldContent, "", to)
		}
	}

	for key, newContent := range newIndex {
		if _, ok := oldIndex[key]; !ok {
			// added
			fmt.Fprintf(to, ansi.Color("%s has been added:", "yellow")+"\n", key)
			printDiff("", newContent, to)
		}
	}
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
