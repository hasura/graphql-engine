package migrate

import (
	"fmt"
	"io"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/spf13/viper"
)

type ProjectMigrate struct {
	ec *cli.ExecutionContext
}

func (p *ProjectMigrate) status(opts ...ProjectMigrationStatusOption) ([]databaseMigration, error) {
	lister := newProjectMigrationsStatus(p.ec)
	if len(opts) == 0 {
		opts = append(opts, StatusAllDatabases())
	}
	return lister.Status(opts...)
}

func (p *ProjectMigrate) StatusJSON(opts ...ProjectMigrationStatusOption) (io.Reader, error) {
	lister := newProjectMigrationsStatus(p.ec)
	if len(opts) == 0 {
		opts = append(opts, StatusAllDatabases())
	}
	return lister.StatusJSON(opts...)
}

func (p *ProjectMigrate) Apply(opts ...ProjectMigrationApplierOption) error {
	applier := newProjectMigrationsApplier(p.ec)
	return applier.Apply(opts...)
}

func NewProjectMigrate(projectDirectory string, opts ...ProjectMigrateOption) (*ProjectMigrate, error) {
	p := &ProjectMigrate{}
	ec := cli.NewExecutionContext()
	ec.ExecutionDirectory = projectDirectory
	ec.Viper = viper.New()

	ec.IsTerminal = false
	ec.Stderr = io.Discard
	ec.Stdout = io.Discard
	if err := ec.Prepare(); err != nil {
		return nil, err
	}
	p.ec = ec
	for _, opt := range opts {
		opt(p)
	}
	if err := ec.Validate(); err != nil {
		return nil, err
	}
	if ec.Config.Version <= cli.V1 {
		return nil, fmt.Errorf("config %v is not supported", ec.Config.Version)
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
