package commands

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strings"

	goyaml "github.com/goccy/go-yaml"

	diffpkg "github.com/hasura/graphql-engine/cli/v2/internal/diff"

	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"

	"github.com/pkg/errors"

	"github.com/aryann/difflib"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/mgutz/ansi"
	"github.com/spf13/cobra"
	v2yaml "gopkg.in/yaml.v2"
)

type MetadataDiffOptions struct {
	EC           *cli.ExecutionContext
	Output       io.Writer
	Args         []string
	DiffType     string
	DisableColor bool
	// two Metadata to diff, 2nd is server if it's empty
	Metadata [2]string
}

func newMetadataDiffCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataDiffOptions{
		EC:     ec,
		Output: ec.Stdout,
	}

	metadataDiffCmd := &cobra.Command{
		Use:   "diff [file1] [file2]",
		Short: "(PREVIEW) Show a highlighted diff of Hasura metadata",
		Long: `(PREVIEW) Show changes between two different sets of Hasura metadata.
By default, it shows changes between the exported metadata file and server metadata`,
		Example: `  # NOTE: This command is in preview, usage and diff format may change.

  # Show changes between server metadata and the exported metadata file:
  hasura metadata diff
  
# Apply admin secret for Hasura GraphQL engine:
  hasura metadata diff --admin-secret "<admin-secret>"

  # Specify a diff type
  hasura metadata diff --type "unified-json"
  hasura metadata diff --type "json"

  # Diff metadata on a different Hasura instance:
  hasura metadata diff --endpoint "<endpoint>"`,
		Args: cobra.MaximumNArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.Args = args
			opts.DisableColor = ec.NoColor
			return opts.Run()
		},
	}

	f := metadataDiffCmd.Flags()

	f.StringVar(&opts.DiffType, "type", "", fmt.Sprintf(`specify a type of diff [allowed values: %v, %v, %v]`, DifftypeUnifiedJSON, DifftypeYAML, DifftypeJSON))

	return metadataDiffCmd
}

func (o *MetadataDiffOptions) Run() error {
	if o.EC.Config.Version >= cli.V2 && o.EC.MetadataDir != "" {
		return getMetadataModeHandler(o.EC.MetadataMode).Diff(o)
	} else {
		return fmt.Errorf("metadata diff for config %d not supported", o.EC.Config.Version)
	}
}

type DiffType string

const DifftypeUnifiedJSON DiffType = "unified-json"
const DifftypeYAML DiffType = "yaml"
const DifftypeJSON DiffType = "json"

const zeroDifferencesFound = "zero differences found"

type printGeneratedMetadataFileDiffOpts struct {
	projectMetadataHandler *projectmetadata.Handler
	// actual directory paths to project directory
	fromProjectDirectory string
	toProjectDirectory   string

	// friendly names if any to both from and to directories
	// for example the diff can between the current project directory and server
	fromFriendlyName string
	toFriendlyName   string

	disableColor bool
	diffType     DiffType
	metadataMode cli.MetadataMode
	writer       io.Writer
	ec           *cli.ExecutionContext
}

func printGeneratedMetadataFileDiffBetweenProjectDirectories(opts printGeneratedMetadataFileDiffOpts) error {
	// build server metadata
	opts.projectMetadataHandler.SetMetadataObjects(projectmetadata.GetMetadataObjectsWithDir(opts.ec, opts.toProjectDirectory))
	serverMeta, err := opts.projectMetadataHandler.BuildMetadata()
	if err != nil {
		return err
	}
	newYaml, err := v2yaml.Marshal(serverMeta)
	if err != nil {
		return errors.Wrap(err, "cannot unmarshall server metadata")
	}
	opts.projectMetadataHandler.SetMetadataObjects(projectmetadata.GetMetadataObjectsWithDir(opts.ec, opts.fromProjectDirectory))
	localMeta, err := opts.projectMetadataHandler.BuildMetadata()
	if err != nil {
		return err
	}
	oldYaml, err := v2yaml.Marshal(localMeta)
	if err != nil {
		return errors.Wrap(err, "cannot unmarshal local metadata")
	}

	switch opts.diffType {
	case DifftypeJSON:
		newJson, err := goyaml.YAMLToJSON(newYaml)
		if err != nil {
			return fmt.Errorf("cannot unmarshal local metadata to json: %w", err)
		}
		oldJson, err := goyaml.YAMLToJSON(oldYaml)
		if err != nil {
			return fmt.Errorf("cannot unmarshal server metadata to json: %w", err)
		}
		var newJsonBuf, oldJsonBuf bytes.Buffer
		err = json.Indent(&newJsonBuf, newJson, "", "  ")
		if err != nil {
			return fmt.Errorf("cannot indent server localmetadata to json: %w", err)
		}
		err = json.Indent(&oldJsonBuf, oldJson, "", "  ")
		if err != nil {
			return fmt.Errorf("cannot indent server metadata to json: %w", err)
		}

		return printMyersDiff(string(newYaml), string(oldYaml), opts.toFriendlyName, opts.fromFriendlyName, opts.writer, opts.disableColor)
	case DifftypeYAML:
		return printMyersDiff(string(newYaml), string(oldYaml), opts.toFriendlyName, opts.fromFriendlyName, opts.writer, opts.disableColor)
	case DifftypeUnifiedJSON:
		printUnifiedJSONDiff(string(newYaml), string(oldYaml), opts.writer)
	}
	return nil
}

func printMyersDiff(before, after, from, to string, writer io.Writer, disableColor bool) error {
	fmt.Fprintf(writer, "- %s\n", diffpkg.MakeDiffLine(from, "red", disableColor))
	fmt.Fprintf(writer, "+ %s\n", diffpkg.MakeDiffLine(to, "green", disableColor))
	count, err := diffpkg.MyersDiff(before, after, from, to, writer, disableColor)
	if err != nil {
		return err
	}
	if count == 0 {
		fmt.Fprintln(writer, zeroDifferencesFound)
	}
	return nil
}

func printUnifiedJSONDiff(before, after string, to io.Writer) {
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

func checkDir(path string) error {
	file, err := os.Stat(path)
	if err != nil {
		return err
	}
	if !file.IsDir() {
		return fmt.Errorf("metadata diff only works with folder but got file %s", path)
	}
	return nil
}
