package migrate

import (
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/commands"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
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
		p.opts.EC.AllDatabases = true
	}
}

func ApplyOnDatabaseName(databaseName string) ProjectMigrationApplierOption {
	return func(p *projectMigrationsApplier) {
		p.opts.Source.Name = databaseName
	}
}

func ApplyWithSkipExecution() ProjectMigrationApplierOption {
	return func(p *projectMigrationsApplier) {
		p.opts.SkipExecution = true
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

func (p *projectMigrationsApplier) apply(opts ...ProjectMigrationApplierOption) ([]ApplyResult, error) {
	var op errors.Op = "migrate.projectMigrationsApplier.apply"
	for _, opt := range opts {
		opt(p)
	}
	var results []ApplyResult
	resultChan, err := p.opts.Apply()
	if err != nil {
		return nil, errors.E(op, err)
	}
	for v := range resultChan {
		results = append(results, ApplyResult(v))
	}
	return results, nil
}
