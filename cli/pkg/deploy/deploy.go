package deploy

import (
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/commands"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

type projectDeployExecutor struct {
	ec   *cli.ExecutionContext
	opts commands.DeployOptions
}

func newProjectDeployExecutor(ec *cli.ExecutionContext) *projectDeployExecutor {
	d := &projectDeployExecutor{
		ec:   ec,
		opts: commands.DeployOptions{EC: ec},
	}
	return d
}

type ProjectDeployExecutorOptions func(executor *projectDeployExecutor)

func WithSeeds() ProjectDeployExecutorOptions {
	return func(p *projectDeployExecutor) {
		p.opts.WithSeeds = true
	}
}

func (d *projectDeployExecutor) deploy(opts ...ProjectDeployExecutorOptions) error {
	var op errors.Op = "deploy.projectDeployExecutor.deploy"
	for _, opt := range opts {
		opt(d)
	}
	err := d.opts.Run()
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}
