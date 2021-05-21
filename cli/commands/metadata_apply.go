package commands

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"

	"github.com/goccy/go-yaml"

	"github.com/hasura/graphql-engine/cli/internal/hasura"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/internal/metadataobject"
	"github.com/spf13/cobra"
)

func newMetadataApplyCmd(ec *cli.ExecutionContext) *cobra.Command {
	opts := &MetadataApplyOptions{
		EC: ec,
	}

	metadataApplyCmd := &cobra.Command{
		Use:   "apply",
		Short: "Apply Hasura metadata on a database",
		Example: `  # Apply Hasura GraphQL engine metadata present in metadata.[yaml|json] file:
  hasura metadata apply

  # Use with admin secret:
  hasura metadata apply --admin-secret "<admin-secret>"

  # Apply metadata to an instance specified by the flag:
  hasura metadata apply --endpoint "<endpoint>"`,
		SilenceUsage: true,
		RunE: func(cmd *cobra.Command, args []string) error {
			if opts.FromFile {
				return fmt.Errorf("use of deprecated flag")
			}
			err := opts.Run()
			if err != nil {
				return err
			}
			return nil
		},
	}

	f := metadataApplyCmd.Flags()

	// deprecated flag
	f.BoolVar(&opts.FromFile, "from-file", false, "apply metadata from migrations/metadata.[yaml|json]")
	f.MarkDeprecated("from-file", "deprecation is a side effect of config v1 deprecation from v2.0.0")

	f.BoolVar(&opts.DryRun, "dry-run", false, "show metadata generated from project directory without applying to server.  generated metadata will be printed as JSON by default, use -o flag for other display formats")
	f.StringVarP(&opts.rawOutput, "output", "o", "", `specify an output format to show applied metadata. Allowed values: json, yaml (default "json")`)
	return metadataApplyCmd
}

type MetadataApplyOptions struct {
	EC *cli.ExecutionContext

	FromFile  bool
	DryRun    bool
	rawOutput string
}

func (o *MetadataApplyOptions) Run() error {
	metadataHandler := metadataobject.NewHandlerFromEC(o.EC)
	// check for any input from pipe
	info, err := os.Stdin.Stat()
	if err != nil {
		o.EC.Logger.Debug("looks look we didn't find a stdin")
	}
	if err == nil {
		if (info.Mode() & os.ModeCharDevice) == 0 {
			reader := bufio.NewReader(os.Stdin)
			o.EC.Logger.Debug("applying metadata from pipe")
			metadataJSON, err := getMetadataJSON(ec, reader)
			if err != nil {
				return err
			}
			if !o.DryRun {
				if err := replaceMetadata(ec, metadataJSON); err != nil {
					return errorApplyingMetadata(err)
				}
				if len(o.rawOutput) != 0 {
					// if not  a dry run after applying metadata, get a dump from server and print into stdout
					return getMetadataFromServerAndWriteToStdoutByFormat(o.EC, rawOutputFormat(o.rawOutput))
				}
				return nil
			} else {
				// on a dry run write the input to stdout by format
				outputFormat := rawOutputFormatJSON
				if len(o.rawOutput) > 0 {
					outputFormat = rawOutputFormat(o.rawOutput)
				}
				return writeByOutputFormat(os.Stdout, metadataJSON, outputFormat)
			}
			return nil
		}
	}

	if !o.DryRun {
		o.EC.Spin("Applying metadata...")
		if o.EC.Config.Version == cli.V2 {
			err := metadataHandler.V1ApplyMetadata()
			o.EC.Spinner.Stop()
			if err != nil {
				return errorApplyingMetadata(err)
			}
			o.EC.Logger.Debug("metadata applied using v1 replace_metadata")
		} else {
			r, err := metadataHandler.V2ApplyMetadata()
			o.EC.Spinner.Stop()
			if err != nil {
				return errorApplyingMetadata(err)
			}
			if !r.IsConsistent {
				o.EC.Logger.Warn("Metadata is inconsistent")
			}
			o.EC.Logger.Debug("metadata applied using v2 replace_metadata")
		}
		if len(o.rawOutput) <= 0 {
			o.EC.Logger.Info("Metadata applied")
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

		if err := writeByOutputFormat(os.Stdout, projectMetadataJSON, rawOutputFormat(o.rawOutput)); err != nil {
			return fmt.Errorf("displaying metadata failed: %w", err)
		}
	}
	return nil
}

// get metadata from reader is it is in JSON/YAML format
// returns an error otherwise
func getMetadataJSON(ec *cli.ExecutionContext, reader io.Reader) ([]byte, error) {
	maybeMetadata, err := ioutil.ReadAll(reader)
	if err != nil {
		return nil, fmt.Errorf("reading input from os.Stdin: %w", err)
	}
	if len(maybeMetadata) == 0 {
		return nil, fmt.Errorf("found 0 bytes from input")
	}
	var metadata []byte
	if isJSON(maybeMetadata) {
		metadata = maybeMetadata
		ec.Logger.Debug("found JSON metadata from pipe")
	} else if isYAML(maybeMetadata) {
		ec.Logger.Debug("found YAML metadata from pipe")
		yamlMetadata, err := yaml.YAMLToJSON(maybeMetadata)
		if err != nil {
			return nil, fmt.Errorf("error decoding input as json: %w", err)
		}
		metadata = yamlMetadata
	} else {
		return nil, fmt.Errorf("cannot determine format from input, accepted input formats: [json,yaml]")
	}
	return metadata, nil
}

func errorApplyingMetadata(err error) error {
	// a helper function to have consistent error messages for errors
	// when applying metadata
	return fmt.Errorf("error applying metadata \n%w", err)
}

func replaceMetadata(ec *cli.ExecutionContext, metadataJSON []byte) error {
	if ec.Config.Version == cli.V2 {
		if _, err := cli.GetCommonMetadataOps(ec).ReplaceMetadata(bytes.NewReader(metadataJSON)); err != nil {
			return err
		}
	} else {
		var v interface{}
		if err := json.Unmarshal(metadataJSON, &v); err != nil {
			return err
		}
		r, err := ec.APIClient.V1Metadata.V2ReplaceMetadata(hasura.V2ReplaceMetadataArgs{
			AllowInconsistentMetadata: true,
			Metadata:                  v,
		})
		if err != nil {
			return err
		}
		if !r.IsConsistent {
			ec.Logger.Warn("Metadata is inconsistent")
		}
	}
	return nil
}
