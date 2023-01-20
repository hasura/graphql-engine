package commands

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strings"

	diffpkg "github.com/hasura/graphql-engine/cli/v2/internal/diff"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"

	"github.com/aryann/difflib"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/mgutz/ansi"
	"github.com/spf13/cobra"
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
		Short: "(PREVIEW) Show a highlighted diff of the Hasura Metadata",
		Long:  "(PREVIEW) This command shows changes between two different sets of Hasura Metadata. By default, it shows changes between the exported Hasura Metadata and the Hasura Metadata on the server.",
		Example: `  # NOTE: This command is in preview, usage and diff format may change.

  # Show changes between server metadata and the exported metadata file:
  hasura metadata diff
  
# Apply admin secret for Hasura GraphQL Engine:
  hasura metadata diff --admin-secret "<admin-secret>"

  # Specify a diff type
  hasura metadata diff --type "unified-json"
  hasura metadata diff --type "json"

  # Diff metadata on a different Hasura instance:
  hasura metadata diff --endpoint "<endpoint>"`,
		Args: cobra.MaximumNArgs(2),
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			if len(opts.DiffType) > 0 {
				optsDiffType := DiffType(opts.DiffType)
				diffTypes := []DiffType{DifftypeJSON, DifftypeYAML, DifftypeUnifiedJSON, DifftypeUnifiedYAML}
				for _, diffType := range diffTypes {
					if optsDiffType == diffType {
						return nil
					}
				}
				return errors.E(op, fmt.Errorf("metadata diff doesn't support difftype %s", optsDiffType))
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			opts.Args = args
			opts.DisableColor = ec.NoColor
			if err := opts.Run(); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
	}

	f := metadataDiffCmd.Flags()

	f.StringVar(&opts.DiffType, "type", "", fmt.Sprintf(`specify a type of diff [allowed values: %v,%v, %v, %v]`, DifftypeUnifiedJSON, DifftypeUnifiedYAML, DifftypeYAML, DifftypeJSON))

	return metadataDiffCmd
}

func (o *MetadataDiffOptions) Run() error {
	var op errors.Op = "commands.MetadataDiffOptions.Run"
	if o.EC.Config.Version >= cli.V2 && o.EC.MetadataDir != "" {
		if err := getMetadataModeHandler(o.EC.MetadataMode).Diff(o); err != nil {
			return errors.E(op, err)
		}
		return nil
	} else {
		return errors.E(op, fmt.Errorf("metadata diff for config %d not supported", o.EC.Config.Version))
	}
}

type DiffType string

const DifftypeUnifiedJSON DiffType = "unified-json"
const DifftypeUnifiedYAML DiffType = "unified-yaml"
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
	var op errors.Op = "commands.printGeneratedMetadataFileDiffBetweenProjectDirectories"
	// build server metadata
	opts.projectMetadataHandler.SetMetadataObjects(projectmetadata.GetMetadataObjectsWithDir(opts.ec, opts.toProjectDirectory))
	newYaml, err := opts.projectMetadataHandler.BuildYAMLMetadata()
	if err != nil {
		return errors.E(op, err)
	}
	opts.projectMetadataHandler.SetMetadataObjects(projectmetadata.GetMetadataObjectsWithDir(opts.ec, opts.fromProjectDirectory))
	oldYaml, err := opts.projectMetadataHandler.BuildYAMLMetadata()
	if err != nil {
		return errors.E(op, err)
	}

	switch opts.diffType {
	case DifftypeJSON:
		newJson, err := convertYamlToJsonWithIndent(newYaml)
		if err != nil {
			return errors.E(op, fmt.Errorf("cannot unmarshal local metadata to json: %w", err))
		}
		oldJson, err := convertYamlToJsonWithIndent(oldYaml)
		if err != nil {
			return errors.E(op, fmt.Errorf("cannot unmarshal server metadata to json: %w", err))
		}

		if err := printMyersDiff(string(newJson), string(oldJson), opts.toFriendlyName, opts.fromFriendlyName, opts.writer, opts.disableColor); err != nil {
			return errors.E(op, err)
		}
		return nil
	case DifftypeYAML:
		if err := printMyersDiff(string(newYaml), string(oldYaml), opts.toFriendlyName, opts.fromFriendlyName, opts.writer, opts.disableColor); err != nil {
			return errors.E(op, err)
		}
		return nil
	case DifftypeUnifiedYAML:
		printUnifiedDiff(string(newYaml), string(oldYaml), opts.writer)
	case DifftypeUnifiedJSON:
		newJson, err := convertYamlToJsonWithIndent(newYaml)
		if err != nil {
			return errors.E(op, fmt.Errorf("cannot unmarshal local metadata to json: %w", err))
		}
		oldJson, err := convertYamlToJsonWithIndent(oldYaml)
		if err != nil {
			return errors.E(op, fmt.Errorf("cannot unmarshal server metadata to json: %w", err))
		}
		printUnifiedDiff(string(newJson), string(oldJson), opts.writer)
	}
	return nil
}

func printMyersDiff(before, after, from, to string, writer io.Writer, disableColor bool) error {
	var op errors.Op = "commands.printMyersDiff"
	fmt.Fprintf(writer, "- %s\n", diffpkg.MakeDiffLine(from, "red", disableColor))
	fmt.Fprintf(writer, "+ %s\n", diffpkg.MakeDiffLine(to, "green", disableColor))
	count, err := diffpkg.MyersDiff(before, after, from, to, writer, disableColor)
	if err != nil {
		return errors.E(op, err)
	}
	if count == 0 {
		fmt.Fprintln(writer, zeroDifferencesFound)
	}
	return nil
}

func printUnifiedDiff(before, after string, to io.Writer) {
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
	var op errors.Op = "commands.checkDir"
	file, err := os.Stat(path)
	if err != nil {
		return errors.E(op, err)
	}
	if !file.IsDir() {
		return errors.E(op, fmt.Errorf("metadata diff only works with folder but got file %s", path))
	}
	return nil
}

func convertYamlToJsonWithIndent(yamlByt []byte) ([]byte, error) {
	var op errors.Op = "commands.convertYamlToJsonWithIndent"
	jsonByt, err := metadatautil.YAMLToJSON(yamlByt)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("cannot convert yaml to json: %w", err))
	}
	var jsonBuf bytes.Buffer
	err = json.Indent(&jsonBuf, jsonByt, "", "  ")
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("cannot indent json: %w", err))
	}
	return jsonBuf.Bytes(), nil
}
