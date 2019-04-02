package commands

import (
	"encoding/json"
	"io/ioutil"
	"os"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

func NewMetadataCmd(ec *cli.ExecutionContext) *cobra.Command {
	metadataCmd := &cobra.Command{
		Use:          "metadata",
		Short:        "Manage Hasura GraphQL Engine metadata saved in the database",
		SilenceUsage: true,
	}
	metadataCmd.AddCommand(
		newMetadataExportCmd(ec),
		newMetadataClearCmd(ec),
		newMetadataReloadCmd(ec),
		newMetadataApplyCmd(ec),
	)
	return metadataCmd
}

func executeMetadata(cmd string, t *migrate.Migrate, ec *cli.ExecutionContext) error {
	switch cmd {
	case "export":
		metaData, err := t.ExportMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot export metadata")
		}

		t, err := json.Marshal(metaData)
		if err != nil {
			return errors.Wrap(err, "cannot Marshal metadata")
		}

		data, err := yaml.JSONToYAML(t)
		if err != nil {
			return err
		}

		metadataPath, err := ec.GetMetadataFilePath("yaml")
		if err != nil {
			return errors.Wrap(err, "cannot save metadata")
		}

		err = ioutil.WriteFile(metadataPath, data, 0644)
		if err != nil {
			return errors.Wrap(err, "cannot save metadata")
		}
	case "clear":
		err := t.ResetMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot clear Metadata")
		}
	case "reload":
		err := t.ReloadMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot reload Metadata")
		}
	case "apply":
		var data interface{}
		var metadataContent []byte
		for _, format := range []string{"yaml", "json"} {
			metadataPath, err := ec.GetMetadataFilePath(format)
			if err != nil {
				return errors.Wrap(err, "cannot apply metadata")
			}

			metadataContent, err = ioutil.ReadFile(metadataPath)
			if err != nil {
				if os.IsNotExist(err) {
					continue
				}
				return err
			}
			break
		}

		if metadataContent == nil {
			return errors.New("Unable to locate metadata.[yaml|json] file under migrations directory")
		}

		err := yaml.Unmarshal(metadataContent, &data)
		if err != nil {
			return errors.Wrap(err, "cannot parse metadata file")
		}

		err = t.ApplyMetadata(data)
		if err != nil {
			return errors.Wrap(err, "cannot apply metadata on the database")
		}
		return nil
	}
	return nil
}
