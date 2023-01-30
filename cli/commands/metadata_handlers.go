package commands

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"
)

type MetadataModeHandler interface {
	Export(*MetadataExportOptions) error
	Apply(*MetadataApplyOptions) error
	Diff(*MetadataDiffOptions) error
}

func getMetadataModeHandler(mode cli.MetadataMode) MetadataModeHandler {
	switch mode {
	case cli.MetadataModeJSON:
		return &metadataModeJSONHandler{}
	case cli.MetadataModeYAML:
		return &metadataModeYAMLHandler{}
	default:
		return &metadataModeDirectoryHandler{}
	}
}

type metadataModeDirectoryHandler struct{}

func (m *metadataModeDirectoryHandler) Export(o *MetadataExportOptions) error {
	var op errors.Op = "commands.metadataModeDirectoryHandler.Export"
	metadataHandler := projectmetadata.NewHandlerFromEC(o.EC)
	files, err := metadataHandler.ExportMetadata()
	o.EC.Spinner.Stop()
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to export metadata: %w", err))
	}
	err = metadataHandler.WriteMetadata(files)
	if err != nil {
		return errors.E(op, fmt.Errorf("cannot write metadata to project: %w", err))
	}
	return nil
}

func (m *metadataModeDirectoryHandler) Apply(o *MetadataApplyOptions) error {
	var op errors.Op = "commands.metadataModeDirectoryHandler.Apply"
	metadataHandler := projectmetadata.NewHandlerFromEC(o.EC)
	if !o.DryRun {
		if o.EC.Config.Version == cli.V2 {
			_, err := metadataHandler.V1ApplyMetadata()
			if err != nil {
				return errors.E(op, errorApplyingMetadata(err))
			}
			o.EC.Logger.Debug("metadata applied using v1 replace_metadata")
		} else {
			r, err := metadataHandler.V2ApplyMetadata(o.DisallowInconsistencies)
			if err != nil {
				return errors.E(op, errorApplyingMetadata(err))
			}
			if !r.IsConsistent {
				o.EC.Logger.Warn("Metadata is inconsistent")
				o.EC.Logger.Warn("Use 'hasura metadata ic list' command to list inconsistent objects")
			}
			o.EC.Logger.Debug("metadata applied using v2 replace_metadata")
		}
		if len(o.rawOutput) != 0 {
			// if not a dry run fetch metadata from and server and print it to stdout
			if err := getMetadataFromServerAndWriteToStdoutByFormat(o.EC, rawOutputFormat(o.rawOutput)); err != nil {
				return errors.E(op, err)
			}
			return nil
		}
		return nil
	}

	if o.DryRun {
		projectMetadataJSON, err := metadataHandler.BuildJSONMetadata()
		if err != nil {
			return errors.E(op, fmt.Errorf("error building project metadata: %w", err))
		}

		if o.DryRun && len(o.rawOutput) == 0 {
			// ie users who probably expect old behaviour
			// show a warning about change in behaviour
			o.rawOutput = string(rawOutputFormatJSON)
			o.EC.Logger.Warn("behaviour of --dry-run flag has changed from v2.0.0. It used to show a diff between metadata on server and local project")
			o.EC.Logger.Warn("new behaviour is to output local project metadata as JSON by default. The output format is configurable by -o flag eg: `hasura metadata apply --dry-run -o yaml`")
			o.EC.Logger.Warn("the old behaviour can be achieved using `hasura metadata diff` command")
		}

		if err := writeByOutputFormat(o.EC.Stdout, projectMetadataJSON, rawOutputFormat(o.rawOutput)); err != nil {
			return errors.E(op, fmt.Errorf("displaying metadata failed: %w", err))
		}
	}
	return nil
}

func (m *metadataModeDirectoryHandler) Diff(o *MetadataDiffOptions) error {
	var op errors.Op = "commands.metadataModeDirectoryHandler.Diff"
	args := o.Args
	messageFormat := "Showing diff between %s and %s..."
	message := ""
	metadataHandler := projectmetadata.NewHandlerFromEC(o.EC)
	fromFriendlyName := "project"
	toFriendlyName := "server"
	fromDirectory, toDirectory := "", ""
	switch len(args) {
	case 0:
		o.Metadata[0] = o.EC.MetadataDir
		fromFriendlyName = "project"
		fromDirectory = o.EC.MetadataDir
	case 1:
		// 1 arg, diff given directory and the metadata on server
		err := checkDir(args[0])
		if err != nil {
			return errors.E(op, err)
		}
		o.Metadata[0] = args[0]
		fromFriendlyName = o.Metadata[0]
		fromDirectory = o.Metadata[0]
	case 2:
		err := checkDir(args[0])
		if err != nil {
			return errors.E(op, err)
		}
		o.Metadata[0] = args[0]
		fromFriendlyName = o.Metadata[0]
		fromDirectory = o.Metadata[0]

		err = checkDir(args[1])
		if err != nil {
			return errors.E(op, err)
		}
		o.Metadata[1] = args[1]
		toFriendlyName = o.Metadata[1]
		toDirectory = o.Metadata[1]
	}

	message = fmt.Sprintf(messageFormat, fromFriendlyName, toFriendlyName)
	o.EC.Logger.Info(message)

	if o.Metadata[1] == "" {
		// if no arguments are provided to `metadata diff` command, then it means
		// we have to compare project directory with the server
		// For that, we are creating a temporary metadata directory and exporting
		// metadata from the server to it and then setting it as the "toDirectory"
		tmpDir, err := ioutil.TempDir("", "*")
		if err != nil {
			return errors.E(op, err)
		}
		defer os.RemoveAll(tmpDir)
		metadataHandler.SetMetadataObjects(projectmetadata.GetMetadataObjectsWithDir(o.EC, tmpDir))
		var files map[string][]byte
		files, err = metadataHandler.ExportMetadata()
		if err != nil {
			return errors.E(op, err)
		}
		err = metadataHandler.WriteMetadata(files)
		if err != nil {
			return errors.E(op, err)
		}
		toDirectory = tmpDir
	}
	if len(o.DiffType) > 0 {
		opts := printGeneratedMetadataFileDiffOpts{
			projectMetadataHandler: metadataHandler,
			fromProjectDirectory:   fromDirectory,
			toProjectDirectory:     toDirectory,
			fromFriendlyName:       fromFriendlyName,
			toFriendlyName:         toFriendlyName,
			disableColor:           o.DisableColor,
			diffType:               DiffType(o.DiffType),
			metadataMode:           o.EC.MetadataMode,
			writer:                 o.Output,
			ec:                     o.EC,
		}
		if err := printGeneratedMetadataFileDiffBetweenProjectDirectories(opts); err != nil {
			return errors.E(op, err)
		}
		return nil
	}
	opts := projectmetadata.PrintContextRichDiffBetweenProjectDirectoriesOpts{
		EC:              o.EC,
		MetadataHandler: metadataHandler,
		FromDirectory:   toDirectory,
		ToDirectory:     fromDirectory,
		Writer:          o.Output,
		DisableColor:    o.DisableColor,
	}
	if err := projectmetadata.PrintContextRichDiffBetweenProjectDirectories(opts); err != nil {
		return errors.E(op, err)
	}
	return nil
}

type metadataModeJSONHandler struct{}

func (m *metadataModeJSONHandler) Export(o *MetadataExportOptions) error {
	var op errors.Op = "commands.metadataModeJSONHandler.Export"
	if err := export(o, o.EC.MetadataMode); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (m *metadataModeJSONHandler) Apply(o *MetadataApplyOptions) error {
	var op errors.Op = "commands.metadataModeJSONHandler.Apply"
	if err := apply(o, o.EC.MetadataMode); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (m *metadataModeJSONHandler) Diff(o *MetadataDiffOptions) error {
	var op errors.Op = "commands.metadataModeJSONHandler.Diff"
	if err := diff(o, DifftypeJSON); err != nil {
		return errors.E(op, err)
	}
	return nil
}

type metadataModeYAMLHandler struct{}

func (m *metadataModeYAMLHandler) Export(o *MetadataExportOptions) error {
	var op errors.Op = "commands.metadataModeYAMLHandler.Export"
	if err := export(o, o.EC.MetadataMode); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (m *metadataModeYAMLHandler) Apply(o *MetadataApplyOptions) error {
	var op errors.Op = "commands.metadataModeYAMLHandler.Apply"
	if err := apply(o, o.EC.MetadataMode); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (m *metadataModeYAMLHandler) Diff(o *MetadataDiffOptions) error {
	var op errors.Op = "commands.metadataModeYAMLHandler.Diff"
	if err := diff(o, DifftypeYAML); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func export(o *MetadataExportOptions, mode cli.MetadataMode) error {
	var op errors.Op = "commands.export"
	metadata, err := ec.APIClient.V1Metadata.ExportMetadata()
	if err != nil {
		return errors.E(op, fmt.Errorf("exporting metadata from server: %w", err))
	}
	var metadataBytes []byte
	metadataBytes, err = ioutil.ReadAll(metadata)
	if err != nil {
		return errors.E(op, fmt.Errorf("reading metadata from response: %w", err))
	}
	if mode == cli.MetadataModeYAML {
		metadataBytes, err = metadatautil.JSONToYAML(metadataBytes)
		if err != nil {
			return errors.E(op, fmt.Errorf("parsing metadata to yaml: %w", err))
		}
	}
	err = ioutil.WriteFile(o.EC.MetadataFile, metadataBytes, os.ModePerm)
	if err != nil {
		return errors.E(op, fmt.Errorf("writing metadata to file: %w", err))
	}
	return nil
}

func apply(o *MetadataApplyOptions, mode cli.MetadataMode) error {
	var op errors.Op = "commands.apply"
	var localMetadataBytes []byte
	var err error

	localMetadataBytes, err = ioutil.ReadFile(o.EC.MetadataFile)
	if err != nil {
		return errors.E(op, fmt.Errorf("reading metadata file: %w", err))
	}
	if o.DryRun {
		if len(o.rawOutput) == 0 {
			o.rawOutput = string(rawOutputFormatJSON)
		}
		if err := writeByOutputFormat(o.EC.Stdout, localMetadataBytes, rawOutputFormat(o.rawOutput)); err != nil {
			return errors.E(op, fmt.Errorf("displaying metadata failed: %w", err))
		}
		return nil
	}
	if mode == cli.MetadataModeYAML {
		localMetadataBytes, err = metadatautil.YAMLToJSON(localMetadataBytes)
		if err != nil {
			return errors.E(op, fmt.Errorf("parsing yaml metadata to json: %w", err))
		}
	}
	var metadata interface{}
	err = json.Unmarshal(localMetadataBytes, &metadata)
	if err != nil {
		return errors.E(op, fmt.Errorf("parsing metadata as json: %w", err))
	}
	if o.EC.Config.Version == cli.V2 {
		_, err := cli.GetCommonMetadataOps(o.EC).ReplaceMetadata(bytes.NewReader(localMetadataBytes))
		if err != nil {
			return errors.E(op, err)
		}
		return nil
	}
	r, err := o.EC.APIClient.V1Metadata.V2ReplaceMetadata(hasura.V2ReplaceMetadataArgs{
		AllowInconsistentMetadata: !o.DisallowInconsistencies,
		Metadata:                  metadata,
	})
	if err != nil {
		return errors.E(op, errorApplyingMetadata(err))
	}
	if !r.IsConsistent {
		o.EC.Logger.Warn("Metadata is inconsistent")
		o.EC.Logger.Warn("Use 'hasura metadata ic list' command to list inconsistent objects")
	}
	if len(o.rawOutput) != 0 {
		// if not a dry run fetch metadata from and server and print it to stdout
		if err := getMetadataFromServerAndWriteToStdoutByFormat(o.EC, rawOutputFormat(o.rawOutput)); err != nil {
			return errors.E(op, err)
		}
		return nil
	}
	return nil
}

func diff(o *MetadataDiffOptions, diffType DiffType) error {
	var op errors.Op = "commands.diff"
	if len(o.Args) > 0 {
		return errors.E(op, fmt.Errorf("expected 0 arguments, found: %v", o.Args))
	}
	serverMetadata, err := cli.GetCommonMetadataOps(o.EC).ExportMetadata()
	if err != nil {
		return errors.E(op, fmt.Errorf("exporting metadata from server: %w", err))
	}
	var serverMetadataBytes []byte
	serverMetadataBytes, err = ioutil.ReadAll(serverMetadata)
	if err != nil {
		return errors.E(op, fmt.Errorf("reading server metadata: %w", err))
	}
	if diffType == DifftypeYAML {
		serverMetadataBytes, err = metadatautil.JSONToYAML(serverMetadataBytes)
		if err != nil {
			return errors.E(op, fmt.Errorf("parsing server metadata as yaml: %w", err))
		}
	}
	localMetadataBytes, err := ioutil.ReadFile(o.EC.MetadataFile)
	if err != nil {
		return errors.E(op, fmt.Errorf("reading local metadata: %w", err))
	}
	if err := printMyersDiff(string(serverMetadataBytes), string(localMetadataBytes), "server", "project", o.Output, o.DisableColor); err != nil {
		return errors.E(op, err)
	}
	return nil
}
