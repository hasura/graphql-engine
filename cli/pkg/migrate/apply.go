package migrate

import (
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/commands"
)

type projectMigrationsApplier struct {
	ec   *cli.ExecutionContext
	opts commands.MigrateApplyOptions
}

func newProjectMigrationsApplier(ec *cli.ExecutionContext) *projectMigrationsApplier {
	p := &projectMigrationsApplier{ec: ec, opts: commands.MigrateApplyOptions{EC: ec}}
	return p
}

type ProjectMigrationApplierOption func(applier *projectMigrationsApplier)

func ApplyOnAllDatabases() ProjectMigrationApplierOption {
	return func(p *projectMigrationsApplier) {
		p.opts.AllDatabases = true
	}
}

func ApplyOnDatabaseName(databaseName string) ProjectMigrationApplierOption {
	return func(p *projectMigrationsApplier) {
		p.opts.Source.Name = databaseName
	}
}

type MigrationDirection string

const MigrationDirectionUp MigrationDirection = "up"
const MigrationDirectionDown MigrationDirection = "down"

func ApplyVersion(version string, direction MigrationDirection) ProjectMigrationApplierOption {
	return func(p *projectMigrationsApplier) {
		p.opts.VersionMigration = version
		p.opts.MigrationType = string(direction)
	}
}

func (p *projectMigrationsApplier) Apply(opts ...ProjectMigrationApplierOption) error {
	for _, opt := range opts {
		opt(p)
	}
	return p.opts.Run()
}
