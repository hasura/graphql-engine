package commands

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/sirupsen/logrus"

	"github.com/aryann/difflib"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/metadata"
	"github.com/mgutz/ansi"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"gopkg.in/yaml.v2"
)

const (
	folderType string = "folder"
	fileType   string = "file"
)

type MetadataDiffOptions struct {
	EC     *cli.ExecutionContext
	Output io.Writer
	Args   []string

	// two Metadata to diff, 2nd is server if it's empty
	Metadata [2]string
}

func newMetadataDiffCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataDiffOptions{
		EC:     ec,
		Output: os.Stdout,
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
  hasura metadata diff metadata.yaml ../old/metadata_old.yaml
	
  # Show changes among multiple metadata folders (v2 config only):
  hasura metadata diff metadata1 metadata2

  # Apply admin secret for Hasura GraphQL Engine:
  hasura metadata diff --admin-secret "<admin-secret>"

  # Diff metadata on a different Hasura instance:
  hasura metadata diff --endpoint "<endpoint>"`,
		Args: cobra.MaximumNArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			opts.Args = args
			return opts.Run()
		},
	}

	return metadataDiffCmd
}

func (o *MetadataDiffOptions) runv2(args []string) error {
	messageFormat := "Showing diff between %s directory and %s..."
	message := ""

	switch len(args) {
	case 0:
		o.Metadata[0] = o.EC.MetadataDir
		message = fmt.Sprintf(messageFormat, o.Metadata[0], "the server")
	case 1:
		// 1 arg, diff given directory and the metadata on server
		err := checkDirAndFile(args[0], folderType)
		if err != nil {
			return err
		}
		o.Metadata[0] = args[0]
		message = fmt.Sprintf(messageFormat, o.Metadata[0], "the server")
	case 2:
		err := checkDirAndFile(args[0], folderType)
		if err != nil {
			return err
		}
		o.Metadata[0] = args[0]
		err = checkDirAndFile(args[1], folderType)
		if err != nil {
			return err
		}
		o.Metadata[1] = args[1]
		if args[0] == args[1] {
			return errors.New("directories passed are the same")
		}
		messageEnd := fmt.Sprintf("the %s directory", o.Metadata[1])
		message = fmt.Sprintf(messageFormat, o.Metadata[0], messageEnd)
	}
	o.EC.Logger.Info(message)
	var oldYaml, newYaml []byte
	migrateDrv, err := migrate.NewMigrate(o.EC, true)
	if err != nil {
		return err
	}
	if o.Metadata[1] == "" {
		tmpDir, err := ioutil.TempDir("", "*")
		if err != nil {
			return err
		}
		defer os.RemoveAll(tmpDir)
		migrate.SetMetadataPluginsWithDir(o.EC, migrateDrv, tmpDir)
		files, err := migrateDrv.ExportMetadata()
		if err != nil {
			return err
		}
		err = migrateDrv.WriteMetadata(files)
		if err != nil {
			return err
		}
	} else {
		migrate.SetMetadataPluginsWithDir(o.EC, migrateDrv, o.Metadata[1])
	}

	// build server metadata
	serverMeta, err := migrateDrv.BuildMetadata()
	if err != nil {
		return err
	}
	newYaml, err = yaml.Marshal(serverMeta)
	if err != nil {
		return errors.Wrap(err, "cannot unmarshall server metadata")
	}

	// build local metadata
	migrate.SetMetadataPluginsWithDir(o.EC, migrateDrv, o.Metadata[0])
	localMeta, err := migrateDrv.BuildMetadata()
	if err != nil {
		return err
	}
	oldYaml, err = yaml.Marshal(localMeta)
	if err != nil {
		return errors.Wrap(err, "cannot unmarshal local metadata")
	}

	printDiff(string(oldYaml), string(newYaml), o.Output, o.EC.Logger)
	return nil
}

func (o *MetadataDiffOptions) runv1(args []string) error {
	messageFormat := "Showing diff between %s and %s..."
	message := ""

	switch len(args) {
	case 0:
		// no args, diff exported metadata and metadata on server
		m := metadata.New(o.EC, o.EC.MigrationDir)
		filename, err := m.GetExistingMetadataFile()
		if err != nil {
			return errors.Wrap(err, "failed getting metadata file")
		}
		o.Metadata[0] = filename
		message = fmt.Sprintf(messageFormat, filename, "the server")
	case 1:
		// 1 arg, diff given filename and the metadata on server
		// we are modifying this since we got
		modifiedFilePath := includeMetadataFileInPath(args[0])
		if modifiedFilePath == "" {
			err := checkDirAndFile(args[0], fileType)
			if err != nil {
				return err
			}
			o.Metadata[0] = args[0]
		} else {
			o.Metadata[0] = modifiedFilePath
		}
		message = fmt.Sprintf(messageFormat, o.Metadata[0], "the server")
	case 2:
		// 2 args, diff given filenames
		modifiedFilePath := includeMetadataFileInPath(args[0])
		if modifiedFilePath == "" {
			err := checkDirAndFile(args[0], fileType)
			if err != nil {
				return err
			}
			o.Metadata[0] = args[0]
		} else {
			o.Metadata[0] = modifiedFilePath
		}
		modifiedFilePath = includeMetadataFileInPath(args[1])
		if modifiedFilePath == "" {
			err := checkDirAndFile(args[1], fileType)
			if err != nil {
				return err
			}
			o.Metadata[1] = args[1]
		} else {
			o.Metadata[1] = modifiedFilePath
		}
		message = fmt.Sprintf(messageFormat, o.Metadata[0], o.Metadata[1])
	}

	o.EC.Logger.Info(message)
	var oldYaml, newYaml []byte
	migrateDrv, err := migrate.NewMigrate(o.EC, true)
	if err != nil {
		return err
	}

	if o.Metadata[1] == "" {
		// get metadata from server
		files, err := migrateDrv.ExportMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot fetch metadata from server")
		}

		// export metadata will always return single file for metadata.yaml
		for _, content := range files {
			newYaml = content
		}
	} else {
		newYaml, err = ioutil.ReadFile(o.Metadata[1])
		if err != nil {
			return errors.Wrap(err, "cannot read file")
		}
	}

	oldYaml, err = ioutil.ReadFile(o.Metadata[0])
	if err != nil {
		return errors.Wrap(err, "cannot read file")
	}

	printDiff(string(oldYaml), string(newYaml), o.Output, o.EC.Logger)
	return nil
}

func (o *MetadataDiffOptions) Run() error {
	if o.EC.Config.Version == cli.V2 && o.EC.MetadataDir != "" {
		return o.runv2(o.Args)
	}
	return o.runv1(o.Args)
}

// Move to using myers diffing algo
func printDiff(before, after string, to io.Writer, logger *logrus.Logger) {
	diffs := difflib.Diff(strings.Split(before, "\n"), strings.Split(after, "\n"))

	isThereADiff := false

	for _, diff := range diffs {
		switch diff.Delta {
		case difflib.LeftOnly:
		case difflib.RightOnly:
			isThereADiff = true
			break
		}
	}

	if isThereADiff {
		logger.Info("displaying diff...")
		fmt.Fprintf(to, "\n")
		for _, diff := range diffs {
			text := diff.Payload
			// TODO: Probably printing just these is not enough,
			// we may need to print 2-3 line before and after these
			// changes to provide more context to the users
			switch diff.Delta {
			case difflib.RightOnly:
				fmt.Fprintf(to, "%s\n", ansi.Color(text, "green"))
			case difflib.LeftOnly:
				fmt.Fprintf(to, "%s\n", ansi.Color(text, "red"))
			}
		}
		return
	}

	logger.Info("no diff")
}

// checkDirAndFile supports either "file" or "folder"
func checkDirAndFile(path, pathType string) error {
	file, err := os.Stat(path)

	if err != nil {
		if os.IsNotExist(err) {
			return fmt.Errorf("this %s does not exist", pathType)
		}
		return err
	}

	if !file.IsDir() && pathType == folderType {
		// This is meant for v2 config
		return fmt.Errorf("metadata diff only works with metadata directories but got %s", path)
	}

	if file.IsDir() && pathType == fileType {
		// This is meant for v1 config and no metadata.yml file wasn't found in the given dir.
		return fmt.Errorf("metadata diff only works with metadata files but got %s", path)
	}

	return nil
}

func includeMetadataFileInPath(path string) string {
	folder, err := os.Stat(path)
	if err != nil {
		return ""
	}
	if !folder.IsDir() {
		return ""
	}
	files, err := ioutil.ReadDir(path)
	if err != nil {
		return ""
	}
	for _, file := range files {
		currentFile := file.Name()
		if currentFile == "metadata.yaml" || currentFile == "metadata.yml" {
			return filepath.Join(path, currentFile)
		}
	}
	return ""
}
