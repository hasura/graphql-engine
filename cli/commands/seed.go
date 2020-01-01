package commands

import (
	"fmt"
	"net/url"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	mig "github.com/hasura/graphql-engine/cli/migrate/cmd"
	"github.com/hasura/graphql-engine/cli/version"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

// NewseedCmd returns the seed command
func NewSeedCmd(ec *cli.ExecutionContext) *cobra.Command {
	seedCmd := &cobra.Command{
		Use:          "seed",
		Short:        "Manage seeds on the database",
		SilenceUsage: true,
	}
	seedCmd.AddCommand(
		newSeedCreateCmd(ec),
		newSeedApplyCmd(ec),
	)
	return seedCmd
}

func newSeed(dir string, db *url.URL, adminSecretValue string, logger *logrus.Logger, v *version.Version, isCmd bool) (*migrate.Migrate, error) {
	dbURL := getDataPath(db, getAdminSecretHeaderName(v), adminSecretValue)
	fileURL := getFilePath(dir)
	t, err := migrate.New(fileURL.String(), dbURL.String(), isCmd, logger)
	if err != nil {
		return nil, errors.Wrap(err, "cannot create seed instance")
	}
	return t, nil
}

// ExecuteMigration runs the actual migration
func ExecuteSeed(cmd string, t *migrate.Migrate, stepOrVersion int64) error {
	var err error

	switch cmd {
	case "up":
		err = mig.UpCmd(t, stepOrVersion)
	case "down":
		err = mig.DownCmd(t, stepOrVersion)
	case "version":
		var direction string
		if stepOrVersion >= 0 {
			direction = "up"
		} else {
			direction = "down"
			stepOrVersion = -(stepOrVersion)
		}
		err = mig.GotoCmd(t, uint64(stepOrVersion), direction)
	default:
		err = fmt.Errorf("Invalid command")
	}

	return err
}
