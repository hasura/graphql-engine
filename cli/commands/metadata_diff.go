package commands

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/aryann/difflib"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hexops/gotextdiff"
	"github.com/hexops/gotextdiff/myers"
	"github.com/hexops/gotextdiff/span"
	"github.com/mgutz/ansi"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"gopkg.in/yaml.v2"
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

  # Show changes between server metadata and that in local_metadata.yaml:
  hasura metadata diff local_metadata.yaml

  # Show changes between metadata from metadata.yaml and metadata_old.yaml:
  hasura metadata diff metadata.yaml metadata_old.yaml

  # Apply admin secret for Hasura GraphQL engine:
  hasura metadata diff --admin-secret "<admin-secret>"

  # For unified diff as the default diff just outputs only the difference:
  hasura metadata diff --type "unified-common"

  # Diff metadata on a different Hasura instance:
  hasura metadata diff --endpoint "<endpoint>"`,
		Args: cobra.MaximumNArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.Args = args
			return opts.Run()
		},
	}

	f := metadataDiffCmd.Flags()

	f.StringVar(&opts.DiffType, "type", "default", fmt.Sprintf(`specify a type of diff [allowed values: %v]`, DifftypeUnifiedCommon))

	return metadataDiffCmd
}

func (o *MetadataDiffOptions) runv2(args []string) error {
	messageFormat := "Showing diff between %s and %s..."
	message := ""
	metadataHandler := metadataobject.NewHandlerFromEC(o.EC)
	from := "project"
	to := "server"
	switch len(args) {
	case 0:
		o.Metadata[0] = o.EC.MetadataDir
		from = "project"
	case 1:
		// 1 arg, diff given directory and the metadata on server
		err := checkDir(args[0])
		if err != nil {
			return err
		}
		o.Metadata[0] = args[0]
		from = o.Metadata[0]
	case 2:
		err := checkDir(args[0])
		if err != nil {
			return err
		}
		o.Metadata[0] = args[0]
		from = o.Metadata[0]

		err = checkDir(args[1])
		if err != nil {
			return err
		}
		o.Metadata[1] = args[1]
		to = o.Metadata[1]
	}
	message = fmt.Sprintf(messageFormat, from, to)
	o.EC.Logger.Info(message)
	var oldYaml, newYaml []byte
	if o.Metadata[1] == "" {
		tmpDir, err := ioutil.TempDir("", "*")
		if err != nil {
			return err
		}
		defer os.RemoveAll(tmpDir)
		metadataHandler.SetMetadataObjects(metadataobject.GetMetadataObjectsWithDir(o.EC, tmpDir))
		var files map[string][]byte
		files, err = metadataHandler.ExportMetadata()
		if err != nil {
			return err
		}
		err = metadataHandler.WriteMetadata(files)
		if err != nil {
			return err
		}
	} else {
		metadataHandler.SetMetadataObjects(metadataobject.GetMetadataObjectsWithDir(o.EC, o.Metadata[1]))
	}

	// build server metadata
	serverMeta, err := metadataHandler.BuildMetadata()
	if err != nil {
		return err
	}
	newYaml, err = yaml.Marshal(serverMeta)
	if err != nil {
		return errors.Wrap(err, "cannot unmarshall server metadata")
	}

	// build local metadata
	metadataHandler.SetMetadataObjects(metadataobject.GetMetadataObjectsWithDir(o.EC, o.Metadata[0]))
	localMeta, err := metadataHandler.BuildMetadata()
	if err != nil {
		return err
	}
	oldYaml, err = yaml.Marshal(localMeta)
	if err != nil {
		return errors.Wrap(err, "cannot unmarshal local metadata")
	}

	err = printDiff(string(oldYaml), string(newYaml), from, to, o.Output, o.DiffType, o.DisableColor)
	if err != nil {
		return err
	}
	return nil
}

func (o *MetadataDiffOptions) Run() error {
	if o.EC.Config.Version >= cli.V2 && o.EC.MetadataDir != "" {
		return o.runv2(o.Args)
	} else {
		return fmt.Errorf("metadata diff for config %d not supported", o.EC.Config.Version)
	}
}

type Difftype string

const DifftypeUnifiedCommon Difftype = "unified-common"

func printDiff(before, after, from, to string, writer io.Writer, difftype string, disableColor bool) error {
	diffType := Difftype(difftype)
	switch diffType {
	case DifftypeUnifiedCommon:
		printDiffv1(before, after, writer)
	default:
		return printDiffv2(before, after, from, to, writer, disableColor)
	}
	return nil
}

func printDiffv2(before, after, from, to string, writer io.Writer, disableColor bool) error {
	edits := myers.ComputeEdits(span.URIFromPath("a.txt"), before, after)
	text := fmt.Sprint(gotextdiff.ToUnified(from, to, before, edits))
	makeDiffLine := func(line, color string) string {
		if disableColor {
			return line
		}
		return ansi.Color(line, color)
	}
	lines := strings.Split(text, "\n")
	for _, line := range lines {
		if line == "" {
			break
		}
		if (string)(line[0]) == "-" {
			fmt.Fprintf(writer, "%s\n", makeDiffLine(line, "red"))
		} else if (string)(line[0]) == "+" {
			fmt.Fprintf(writer, "%s\n", makeDiffLine(line, "yellow"))
		} else if (string)(line[0]) == "@" {
			fmt.Fprintf(writer, "%s\n", makeDiffLine(line, "cyan"))
		}
	}

	return nil
}

func printDiffv1(before, after string, to io.Writer) {
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
