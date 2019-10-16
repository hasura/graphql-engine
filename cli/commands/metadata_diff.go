package commands

import (
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/aryann/difflib"
	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	"github.com/mgutz/ansi"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func newMetadataDiffCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &metadataDiffOptions{
		EC:         ec,
		output:     os.Stdout,
	}

	metadataDiffCmd := &cobra.Command{
		Use:   "diff [file1] [file2]",
		Short: "Show changes between local and serverside Hasura metadata",
		Long: `Show changes between two different sets of Hasura metadata.
By default, shows changes between migrations/metadata.[yaml|json] and server metadata.`,
		Example: `  # Show changes between server metadata and that in migrations/metadata.[yaml|json]:
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
			var yamlFileNames [2]string
			if len(args) >= 1 {
				opts.metaDataFiles[0] = args[0]
				yamlFileNames[0] = args[0]
			} else {
				yamlFileNames[0] = "migrations/metadata.yaml"
			}
			if len(args) >= 2 {
				opts.metaDataFiles[1] = args[1]
				yamlFileNames[1] = args[1]
			} else {
				yamlFileNames[1] = "metadata from the server"
			}


			ec.Logger.Info(fmt.Sprintf("Showing diff between %s and %s...\n", yamlFileNames[0], yamlFileNames[1]))
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

type metadataDiffOptions struct {
	EC *cli.ExecutionContext
	output io.Writer

	// Args
	metaDataFiles [2]string
}

func (o *metadataDiffOptions) run() error {
	var oldYaml, newYaml []byte
	migrateDrv, err := newMigrate(o.EC.MigrationDir, o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.Logger, o.EC.Version, true)
	if err != nil {
		return err
	}

	if o.metaDataFiles[0] == "" {
		for _, format := range []string{"yaml", "json"} {
			metadataPath, err := ec.GetMetadataFilePath(format)
			if err != nil {
				return errors.Wrap(err, "cannot diff metadata")
			}

			oldYaml, err = ioutil.ReadFile(metadataPath)
			if err != nil {
				if os.IsNotExist(err) {
					continue
				}
				return err
			}
			break
		}

		if oldYaml == nil {
			return errors.New("Unable to locate metadata.[yaml|json] file under migrations directory")
		}
	} else {
		oldYaml, err = ioutil.ReadFile(o.metaDataFiles[0])
		if err != nil {
			return errors.Wrap(err, fmt.Sprintf("cannot read from file %s", o.metaDataFiles[0]))
		}
	}

	if o.metaDataFiles[1] == "" {
		metaData, err := migrateDrv.ExportMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot fetch metadata from server")
		}

		t, err := json.Marshal(metaData)
		if err != nil {
			return errors.Wrap(err, "cannot Marshal metadata")
		}

		newYaml, err = yaml.JSONToYAML(t)
		if err != nil {
			return err
		}
	} else {
		newYaml, err = ioutil.ReadFile(o.metaDataFiles[1])
		if err != nil {
			return errors.Wrap(err, fmt.Sprintf("cannot read from file %s", o.metaDataFiles[1]))
		}
	}
	return diffYaml(oldYaml, newYaml, o.output)
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
			fmt.Fprintf(to, "%s\n", ansi.Color("+ "+text, "green"))
		case difflib.LeftOnly:
			fmt.Fprintf(to, "%s\n", ansi.Color("- "+text, "red"))
		case difflib.Common:
			fmt.Fprintf(to, "%s\n", "  "+text)
		}
	}
}