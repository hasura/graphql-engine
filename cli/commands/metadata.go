package commands

import (
	"encoding/json"
	"io/ioutil"

	"github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func NewMetadataCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	metadataCmd := &cobra.Command{
		Use:          "metadata",
		Short:        "Manage Hasura GraphQL Engine metdata saved in the database",
		SilenceUsage: true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
	}
	metadataCmd.AddCommand(
		newMetadataExportCmd(ec),
		newMetadataResetCmd(ec),
		newMetadataApplyCmd(ec),
	)
	f := metadataCmd.PersistentFlags()
	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("access_key", f.Lookup("access-key"))
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
