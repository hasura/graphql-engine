package commands

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"path/filepath"

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
		newMetadataResetCmd(ec),
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

		// Check if yaml format supported for metadata file
		metadataPath, err := ec.GetMetadataPath("yaml")
		if err != nil {
			return errors.Wrap(err, "cannot save metadata")
		}

		err = ioutil.WriteFile(metadataPath, data, 0644)
		if err != nil {
			return errors.Wrap(err, "cannot save metadata")
		}
	case "reset":
		err := t.ResetMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot reset Metadata")
		}
	case "reload":
		err := t.ReloadMetadata()
		if err != nil {
			return errors.Wrap(err, "cannot reload Metadata")
		}
	case "apply":
		var data interface{}
		var fileExists bool
		for _, format := range []string{"yaml", "json"} {
			metadataPath, err := ec.GetMetadataPath(format)
			if err != nil {
				return errors.Wrap(err, "cannot save metadata")
			}

			data, err = getMetadataByte(metadataPath)
			if err != nil {
				if os.IsNotExist(err) {
					continue
				}
				return err
			}
			fileExists = true
		}

		if !fileExists {
			return errors.New("Unable to locate metadata.yaml|json file under migrations directory")
		}

		err := t.ApplyMetadata(data)
		if err != nil {
			return errors.Wrap(err, "cannot apply metadata on the database")
		}
		return nil
	}
	return nil
}

func getMetadataByte(path string) (metadata interface{}, err error) {
	data, err := ioutil.ReadFile(path)
	if err != nil {
		return metadata, err
	}

	switch p := filepath.Ext(path); p {
	case ".yaml":
		err = yaml.Unmarshal(data, &metadata)
		if err != nil {
			return metadata, errors.Wrap(err, "cannot parse metadata file")
		}
		return metadata, nil
	case ".json":
		err = json.Unmarshal(data, &metadata)
		if err != nil {
			return metadata, errors.Wrap(err, "cannot parse metadata file")
		}
		return metadata, nil
	}
	return metadata, errors.New("Invalid file extension")
}
