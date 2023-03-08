package deploy

import (
	"fmt"
	"io"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/viper"
)

type ProjectDeploy struct {
	ec *cli.ExecutionContext
}

type ProjectDeployOption func(deploy *ProjectDeploy)

func (p *ProjectDeploy) Deploy(opts ...ProjectDeployExecutorOptions) error {
	var op errors.Op = "deploy.ProjectDeploy.Deploy"
	executor := newProjectDeployExecutor(p.ec)
	err := executor.deploy(opts...)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func WithAdminSecret(adminSecret string) ProjectDeployOption {
	return func(deploy *ProjectDeploy) {
		deploy.ec.Viper.Set("admin_secret", adminSecret)
	}
}

func WithEndpoint(endpoint string) ProjectDeployOption {
	return func(deploy *ProjectDeploy) {
		deploy.ec.Viper.Set("endpoint", endpoint)
	}
}

func WithCliExtPath(path string) ProjectDeployOption {
	return func(d *ProjectDeploy) {
		d.ec.CliExtSourceBinPath = path
	}
}

func WithLogger(logger *logrus.Logger) ProjectDeployOption {
	return func(p *ProjectDeploy) {
		p.ec.Logger = logger
	}
}

func NewProjectDeploy(projectDirectory string, opts ...ProjectDeployOption) (*ProjectDeploy, error) {
	var op errors.Op = "deploy.NewProjectDeploy"
	ec := cli.NewExecutionContext()
	ec.ExecutionDirectory = projectDirectory
	ec.Viper = viper.New()
	ec.IsTerminal = false
	ec.Stdout = io.Discard
	ec.Stderr = io.Discard

	if err := ec.Prepare(); err != nil {
		return nil, errors.E(op, err)
	}
	d := &ProjectDeploy{ec}
	for _, opt := range opts {
		opt(d)
	}

	if err := ec.Validate(); err != nil {
		return nil, errors.E(op, err)
	}
	if ec.Config.Version <= cli.V1 {
		return nil, errors.E(op, fmt.Errorf("config %v is not supported", ec.Config.Version))
	}
	return d, nil
}
