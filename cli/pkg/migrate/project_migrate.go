package migrate

import (
	"fmt"
	"io"

	"github.com/hasura/graphql-engine/cli/v2/commands"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/spf13/viper"
)

type ProjectMigrate struct {
	ec *cli.ExecutionContext
}

func (p *ProjectMigrate) status(opts ...ProjectMigrationStatusOption) ([]databaseMigration, error) {
	var op errors.Op = "migrate.ProjectMigrate.status"
	lister := newProjectMigrationsStatus(p.ec)
	if len(opts) == 0 {
		opts = append(opts, StatusAllDatabases())
	}
	dms, err := lister.Status(opts...)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return dms, nil
}

func (p *ProjectMigrate) StatusJSON(opts ...ProjectMigrationStatusOption) (io.Reader, error) {
	var op errors.Op = "migrate.ProjectMigrate.StatusJSON"
	lister := newProjectMigrationsStatus(p.ec)
	if len(opts) == 0 {
		opts = append(opts, StatusAllDatabases())
	}
	r, err := lister.StatusJSON(opts...)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

type ApplyResult commands.MigrateApplyResult

func (p *ProjectMigrate) Apply(opts ...ProjectMigrationApplierOption) ([]ApplyResult, error) {
	var op errors.Op = "migrate.ProjectMigrate.Apply"
	applier := newProjectMigrationsApplier(p.ec)
	r, err := applier.apply(opts...)
	if err != nil {
		return nil, errors.E(op, err)
	}
	return r, nil
}

func (p *ProjectMigrate) Delete(opts ...ProjectMigrationDeleterOption) error {
	var op errors.Op = "migrate.ProjectMigrate.Delete"
	deleter := newProjectMigrationsDeleter(p.ec)
	if err := deleter.delete(opts...); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func NewProjectMigrate(projectDirectory string, opts ...ProjectMigrateOption) (*ProjectMigrate, error) {
	var op errors.Op = "migrate.NewProjectMigrate"
	p := &ProjectMigrate{}
	ec := cli.NewExecutionContext()
	ec.ExecutionDirectory = projectDirectory
	ec.Viper = viper.New()

	ec.IsTerminal = false
	ec.Stderr = io.Discard
	ec.Stdout = io.Discard
	if err := ec.Prepare(); err != nil {
		return nil, errors.E(op, err)
	}
	p.ec = ec
	for _, opt := range opts {
		opt(p)
	}
	if err := ec.Validate(); err != nil {
		return nil, errors.E(op, err)
	}
	if ec.Config.Version <= cli.V1 {
		return nil, errors.E(op, fmt.Errorf("config %v is not supported", ec.Config.Version))
	}
	return p, nil
}

type ProjectMigrateOption func(*ProjectMigrate)

func WithEndpoint(endpoint string) ProjectMigrateOption {
	return func(m *ProjectMigrate) {
		m.ec.Viper.Set("endpoint", endpoint)
	}
}
func WithAdminSecret(adminSecret string) ProjectMigrateOption {
	return func(m *ProjectMigrate) {
		m.ec.Viper.Set("admin_secret", adminSecret)
	}
}
