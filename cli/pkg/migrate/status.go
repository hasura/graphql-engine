package migrate

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/commands"
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
	var migrateStatus []databaseMigration
	for _, opt := range opts {
		opt(p)
	}
	if p.allDatabases {
		metadataOps := cli.GetCommonMetadataOps(p.ec)
		sources, err := metadatautil.GetSourcesAndKind(metadataOps.ExportMetadata)
		if err != nil {
			return nil, err
		}
		for _, source := range sources {
			opts := commands.MigrateStatusOptions{
				EC: p.ec,
				Source: cli.Source{
					Name: source.Name,
					Kind: source.Kind,
				},
			}
			status, err := opts.Run()
			if err != nil {
				return nil, err
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
	d, err := p.Status(opts...)
	b := new(bytes.Buffer)
	if err != nil {
		return nil, err
	}
	if err := json.NewEncoder(b).Encode(d); err != nil {
		return nil, fmt.Errorf("error encoding migration status as json: %w", err)
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
