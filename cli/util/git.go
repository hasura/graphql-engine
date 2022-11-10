package util

import (
	"io/fs"
	"os"
	"path/filepath"

	"github.com/sirupsen/logrus"
	"gopkg.in/src-d/go-git.v4"
	"gopkg.in/src-d/go-git.v4/config"
	"gopkg.in/src-d/go-git.v4/plumbing"

	stderrors "errors"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

// Default Codegen Assets constants
const (
	ActionsCodegenOrg     string = "hasura/codegen-assets"
	ActionsCodegenRepoURI string = "https://github.com/hasura/codegen-assets.git"
	ActionsCodegenDirName string = "actions-codegen-assets"
)

type GitUtil struct {
	URI  string
	Path string

	// Optional
	ReferenceName        plumbing.ReferenceName
	DisableCloneOrUpdate bool
	Logger               *logrus.Logger
}

func NewGitUtil(uri string, path string, refName string) *GitUtil {
	cfg := &GitUtil{
		URI:           uri,
		Path:          path,
		ReferenceName: plumbing.HEAD,
	}
	if refName != "" {
		cfg.ReferenceName = plumbing.NewBranchReferenceName(refName)
	}
	return cfg
}

func (g *GitUtil) EnsureCloned() error {
	var op errors.Op = "util.GitUtil.EnsureCloned"
	if g.DisableCloneOrUpdate {
		g.Logger.Debugf("skipping clone/update for %s", g.URI)
		return nil
	}
	if ok, err := g.IsGitCloned(); err != nil {
		return errors.E(op, err)
	} else if !ok {
		_, err := git.PlainClone(g.Path, false, &git.CloneOptions{
			URL:           g.URI,
			ReferenceName: g.ReferenceName,
		})
		if err != nil && err != git.ErrRepositoryAlreadyExists {
			return errors.E(op, err)
		}
	}
	return nil
}

func (g *GitUtil) IsGitCloned() (bool, error) {
	var op errors.Op = "util.GitUtil.IsGitCloned"
	f, err := os.Stat(filepath.Join(g.Path, ".git"))
	if stderrors.Is(err, fs.ErrNotExist) {
		return false, nil
	}
	if err != nil {
		return false, errors.E(op, err)
	}
	return f.IsDir(), nil
}

// EnsureUpdated will ensure the destination path exists and is up to date.
func (g *GitUtil) EnsureUpdated() error {
	var op errors.Op = "util.GitUtil.EnsureUpdated"
	if g.DisableCloneOrUpdate {
		g.Logger.Debugf("skipping clone/update for %s", g.URI)
		return nil
	}
	if err := g.EnsureCloned(); err != nil {
		return errors.E(op, err)
	}
	if err := g.updateAndCleanUntracked(); err != nil {
		return errors.E(op, err)
	}
	return nil
}

func (g *GitUtil) updateAndCleanUntracked() error {
	var op errors.Op = "util.GitUtil.updateAndCleanUntracked"
	repo, err := git.PlainOpen(g.Path)
	if err != nil {
		return errors.E(op, err)
	}
	err = repo.Fetch(&git.FetchOptions{
		RefSpecs: []config.RefSpec{"refs/*:refs/*"},
	})
	if err != nil && err != git.NoErrAlreadyUpToDate {
		return errors.E(op, err)
	}
	wt, err := repo.Worktree()
	if err != nil {
		return errors.E(op, err)
	}
	err = wt.Checkout(&git.CheckoutOptions{
		Branch: g.ReferenceName,
	})
	if err != nil {
		return errors.E(op, err)
	}
	err = wt.Pull(&git.PullOptions{
		ReferenceName: g.ReferenceName,
		Force:         true,
	})
	if err != nil && err != git.NoErrAlreadyUpToDate {
		return errors.E(op, err)
	}
	err = wt.Reset(&git.ResetOptions{
		Mode: git.HardReset,
	})
	if err != nil {
		return errors.E(op, err)
	}
	err = wt.Clean(&git.CleanOptions{
		Dir: true,
	})
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}
