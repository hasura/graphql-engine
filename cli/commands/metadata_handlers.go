package commands

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	goyaml "github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"
	"github.com/pkg/errors"
	"gopkg.in/yaml.v2"
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
	metadataHandler := projectmetadata.NewHandlerFromEC(o.EC)
	files, err := metadataHandler.ExportMetadata()
	o.EC.Spinner.Stop()
	if err != nil {
		return errors.Wrap(err, "failed to export metadata")
	}
	err = metadataHandler.WriteMetadata(files)
	if err != nil {
		return errors.Wrap(err, "cannot write metadata to project")
	}
	return nil
}

func (m *metadataModeDirectoryHandler) Apply(o *MetadataApplyOptions) error {
	metadataHandler := projectmetadata.NewHandlerFromEC(o.EC)
	if !o.DryRun {
		if o.EC.Config.Version == cli.V2 {
			_, err := metadataHandler.V1ApplyMetadata()
			if err != nil {
				return errorApplyingMetadata(err)
			}
			o.EC.Logger.Debug("metadata applied using v1 replace_metadata")
		} else {
			r, err := metadataHandler.V2ApplyMetadata()
			if err != nil {
				return errorApplyingMetadata(err)
			}
			if !r.IsConsistent {
				o.EC.Logger.Warn("Metadata is inconsistent")
				o.EC.Logger.Warn("Use 'hasura metadata ic list' command to list inconsistent objects")
			}
			o.EC.Logger.Debug("metadata applied using v2 replace_metadata")
		}
		if len(o.rawOutput) != 0 {
			// if not a dry run fetch metadata from and server and print it to stdout
			return getMetadataFromServerAndWriteToStdoutByFormat(o.EC, rawOutputFormat(o.rawOutput))
		}
		return nil
	}

	if o.DryRun {
		projectMetadataJSON, err := metadataHandler.MakeJSONMetadata()
		if err != nil {
			return fmt.Errorf("error building project metadata: %w", err)
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
			return fmt.Errorf("displaying metadata failed: %w", err)
		}
	}
	return nil
}

func (m *metadataModeDirectoryHandler) Diff(o *MetadataDiffOptions) error {
	args := o.Args
	messageFormat := "Showing diff between %s and %s..."
	message := ""
	metadataHandler := projectmetadata.NewHandlerFromEC(o.EC)
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
		metadataHandler.SetMetadataObjects(projectmetadata.GetMetadataObjectsWithDir(o.EC, tmpDir))
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
		metadataHandler.SetMetadataObjects(projectmetadata.GetMetadataObjectsWithDir(o.EC, o.Metadata[1]))
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
	metadataHandler.SetMetadataObjects(projectmetadata.GetMetadataObjectsWithDir(o.EC, o.Metadata[0]))
	localMeta, err := metadataHandler.BuildMetadata()
	if err != nil {
		return err
	}
	oldYaml, err = yaml.Marshal(localMeta)
	if err != nil {
		return errors.Wrap(err, "cannot unmarshal local metadata")
	}

	// Here oldYaml is project's metadata and newYaml is server's metadata for having diff similar to git diff i.e taking server has base before has been taken as server's metadata
	err = printDiff(string(newYaml), string(oldYaml), to, from, o.Output, o.DiffType, o.DisableColor)
	if err != nil {
		return err
	}
	return nil
}

type metadataModeJSONHandler struct{}

func (m *metadataModeJSONHandler) Export(o *MetadataExportOptions) error {
	return export(o, o.EC.MetadataMode)
}

func (m *metadataModeJSONHandler) Apply(o *MetadataApplyOptions) error {
	return apply(o, o.EC.MetadataMode)
}

func (m *metadataModeJSONHandler) Diff(o *MetadataDiffOptions) error {
	return diff(o, o.EC.MetadataMode)
}

type metadataModeYAMLHandler struct{}

func (m *metadataModeYAMLHandler) Export(o *MetadataExportOptions) error {
	return export(o, o.EC.MetadataMode)
}

func (m *metadataModeYAMLHandler) Apply(o *MetadataApplyOptions) error {
	return apply(o, o.EC.MetadataMode)
}

func (m *metadataModeYAMLHandler) Diff(o *MetadataDiffOptions) error {
	return diff(o, o.EC.MetadataMode)
}

func export(o *MetadataExportOptions, mode cli.MetadataMode) error {
	metadata, err := ec.APIClient.V1Metadata.ExportMetadata()
	if err != nil {
		return fmt.Errorf("exporting metadata from server: %w", err)
	}
	var metadataBytes []byte
	metadataBytes, err = ioutil.ReadAll(metadata)
	if err != nil {
		return fmt.Errorf("reading metadata from response: %w", err)
	}
	if mode == cli.MetadataModeYAML {
		metadataBytes, err = goyaml.JSONToYAML(metadataBytes)
		if err != nil {
			return fmt.Errorf("parsing metadata to yaml: %w", err)
		}
	}
	err = ioutil.WriteFile(o.EC.MetadataFile, metadataBytes, os.ModePerm)
	if err != nil {
		return fmt.Errorf("writing metadata to file: %w", err)
	}
	return nil
}
func apply(o *MetadataApplyOptions, mode cli.MetadataMode) error {
	var localMetadataBytes []byte
	var err error

	localMetadataBytes, err = ioutil.ReadFile(o.EC.MetadataFile)
	if err != nil {
		return fmt.Errorf("reading metadata file: %w", err)
	}
	if o.DryRun {
		if len(o.rawOutput) == 0 {
			o.rawOutput = string(rawOutputFormatJSON)
		}
		if err := writeByOutputFormat(o.EC.Stdout, localMetadataBytes, rawOutputFormat(o.rawOutput)); err != nil {
			return fmt.Errorf("displaying metadata failed: %w", err)
		}
		return nil
	}
	if mode == cli.MetadataModeYAML {
		localMetadataBytes, err = goyaml.YAMLToJSON(localMetadataBytes)
		if err != nil {
			return fmt.Errorf("parsing yaml metadata to json: %w", err)
		}
	}
	var metadata interface{}
	err = json.Unmarshal(localMetadataBytes, &metadata)
	if err != nil {
		return fmt.Errorf("parsing metadata as json: %w", err)
	}
	if o.EC.Config.Version == cli.V2 {
		_, err := cli.GetCommonMetadataOps(o.EC).ReplaceMetadata(bytes.NewReader(localMetadataBytes))
		if err != nil {
			return err
		}
		return nil
	}
	r, err := o.EC.APIClient.V1Metadata.V2ReplaceMetadata(hasura.V2ReplaceMetadataArgs{
		AllowInconsistentMetadata: true,
		Metadata:                  metadata,
	})
	if err != nil {
		return errorApplyingMetadata(err)
	}
	if !r.IsConsistent {
		o.EC.Logger.Warn("Metadata is inconsistent")
		o.EC.Logger.Warn("Use 'hasura metadata ic list' command to list inconsistent objects")
	}
	if len(o.rawOutput) != 0 {
		// if not a dry run fetch metadata from and server and print it to stdout
		return getMetadataFromServerAndWriteToStdoutByFormat(o.EC, rawOutputFormat(o.rawOutput))
	}
	return nil
}

func diff(o *MetadataDiffOptions, mode cli.MetadataMode) error {
	if len(o.Args) > 0 {
		return fmt.Errorf("expected 0 arguments, found: %v", o.Args)
	}
	serverMetadata, err := cli.GetCommonMetadataOps(o.EC).ExportMetadata()
	if err != nil {
		return fmt.Errorf("exporting metadata from server: %w", err)
	}
	var serverMetadataBytes []byte
	serverMetadataBytes, err = ioutil.ReadAll(serverMetadata)
	if err != nil {
		return fmt.Errorf("reading server metadata: %w", err)
	}
	if mode == cli.MetadataModeYAML {
		serverMetadataBytes, err = goyaml.JSONToYAML(serverMetadataBytes)
		if err != nil {
			return fmt.Errorf("parsing server metadata as yaml: %w", err)
		}
	}
	localMetadataBytes, err := ioutil.ReadFile(o.EC.MetadataFile)
	if err != nil {
		return fmt.Errorf("reading local metadata: %w", err)
	}
	return printDiff(string(serverMetadataBytes), string(localMetadataBytes), "server", "project", o.Output, o.DiffType, !o.EC.IsTerminal)
}
