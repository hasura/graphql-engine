package migrate

import (
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/commands"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
)

type projectMigrationsDeleter struct {
	ec *cli.ExecutionContext
	opts commands.MigrateDeleteOptions
}

func newProjectMigrationsDeleter(ec *cli.ExecutionContext) *projectMigrationsDeleter {
	p := &projectMigrationsDeleter{ec: ec, opts: commands.MigrateDeleteOptions{EC: ec}}
	return p
}

type ProjectMigrationDeleterOption func(deleter *projectMigrationsDeleter)

func DeleteOnDatabase(databaseName string, databaseKind hasura.SourceKind) ProjectMigrationDeleterOption {
	return func(p *projectMigrationsDeleter) {
		p.opts.EC.Source.Name = databaseName
		p.opts.EC.Source.Kind = databaseKind
	}
}

func DeleteVersion(version uint64) ProjectMigrationDeleterOption {
	return func(p *projectMigrationsDeleter) {
		p.opts.Version = version
	}
}

func DeleteAllMigrations() ProjectMigrationDeleterOption {
	return func(p *projectMigrationsDeleter) {
		p.opts.All = true
	}
}

func DeleteOnlyOnServer() ProjectMigrationDeleterOption {
	return func(p *projectMigrationsDeleter) {
		p.opts.OnlyServer = true
	}
}

func DeleteOnAllDatabases() ProjectMigrationDeleterOption {
	return func(p *projectMigrationsDeleter) {
		p.opts.EC.AllDatabases = true
	}
}

func (p *projectMigrationsDeleter) delete(opts ...ProjectMigrationDeleterOption) error {
	var op errors.Op = "migrate.projectMigrationsDeleter.delete"
	p.opts.EC.AllDatabases = false // this becomes `true` if not explicitly set to `false` for some unkown reason
	for _, opt := range opts {
		opt(p)
	}
	if err := p.opts.Run(); err != nil {
		return errors.E(op, err)
	}
	return nil
}
