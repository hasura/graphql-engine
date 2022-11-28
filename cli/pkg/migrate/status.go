package migrate

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/commands"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/hasura/graphql-engine/cli/v2/migrate"
)

type databaseMigration struct {
	DatabaseName string         `json:"databaseName"`
	Status       migrate.Status `json:"status"`
}

type projectMigrationsStatus struct {
	ec           *cli.ExecutionContext
	allDatabases bool
}

func (p *projectMigrationsStatus) Status(opts ...ProjectMigrationStatusOption) ([]databaseMigration, error) {
	var op errors.Op = "migrate.projectMigrationsStatus.Status"
	var migrateStatus []databaseMigration
	for _, opt := range opts {
		opt(p)
	}
	if p.allDatabases {
		metadataOps := cli.GetCommonMetadataOps(p.ec)
		sources, err := metadatautil.GetSourcesAndKindStrict(metadataOps.ExportMetadata)
		if err != nil {
			return nil, errors.E(op, err)
		}
		for _, source := range sources {
			opts := commands.MigrateStatusOptions{
				EC: p.ec,
				Source: cli.Source{
					Name: source.Name,
					Kind: source.Kind,
				},
			}
			status, err := opts.RunOnSource()
			if err != nil {
				return nil, errors.E(op, err)
			}
			migrateStatus = append(
				migrateStatus,
				databaseMigration{
					DatabaseName: source.Name,
					Status:       *status,
				},
			)
		}
	}
	return migrateStatus, nil
}

func (p *projectMigrationsStatus) StatusJSON(opts ...ProjectMigrationStatusOption) (io.Reader, error) {
	var op errors.Op = "migrate.projectMigrationsStatus.StatusJSON"
	d, err := p.Status(opts...)
	b := new(bytes.Buffer)
	if err != nil {
		return nil, errors.E(op, err)
	}
	if err := json.NewEncoder(b).Encode(d); err != nil {
		return nil, errors.E(op, fmt.Errorf("error encoding migration status as json: %w", err))
	}
	return b, nil
}

type ProjectMigrationStatusOption func(applier *projectMigrationsStatus)

func newProjectMigrationsStatus(ec *cli.ExecutionContext) *projectMigrationsStatus {
	p := &projectMigrationsStatus{ec: ec}
	return p
}
func StatusAllDatabases() ProjectMigrationStatusOption {
	return func(p *projectMigrationsStatus) {
		p.allDatabases = true
	}
}
