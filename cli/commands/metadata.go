package commands

import (
	"encoding/json"
	"io/ioutil"

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

func executeMetadata(cmd string, t *migrate.Migrate, metadataPath string) error {
	switch cmd {
	case "export":
		metaData, err := t.ExportMetadata()
		if err != nil {
			return errors.Wrap(err, "Cannot export metadata")
		}

		t, err := json.Marshal(metaData)
		if err != nil {
			return errors.Wrap(err, "Cannot Marshal metadata")
		}

		data, err := yaml.JSONToYAML(t)
		if err != nil {
			return err
		}

		err = ioutil.WriteFile(metadataPath, data, 0644)
		if err != nil {
			return errors.Wrap(err, "cannot save metadata")
		}
	case "reset":
		err := t.ResetMetadata()
		if err != nil {
			return errors.Wrap(err, "Cannot reset Metadata")
		}
	case "reload":
		err := t.ReloadMetadata()
		if err != nil {
			return errors.Wrap(err, "Cannot reload Metadata")
		}
	case "apply":
		data, err := ioutil.ReadFile(metadataPath)
		if err != nil {
			return errors.Wrap(err, "cannot read metadata file")
		}

		var q interface{}
		err = yaml.Unmarshal(data, &q)
		if err != nil {
			return errors.Wrap(err, "cannot parse metadata file")
		}

		err = t.ApplyMetadata(q)
		if err != nil {
			return errors.Wrap(err, "cannot apply metadata on the database")
		}
	}
	return nil
}
