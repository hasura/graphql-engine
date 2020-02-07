package gitutil

import (
	"os"
	"path/filepath"

	"gopkg.in/src-d/go-git.v4"
	"gopkg.in/src-d/go-git.v4/plumbing"
)

const (
	indexURI string = "https://github.com/hasura/plugins-index.git"
)

// EnsureCloned will clone into the destination path, otherwise will return no error.
func EnsureCloned(destinationPath string) error {
	if ok, err := IsGitCloned(destinationPath); err != nil {
		return err
	} else if !ok {
		_, err := git.PlainClone(destinationPath, false, &git.CloneOptions{
			URL:           indexURI,
			ReferenceName: plumbing.NewBranchReferenceName("demo"),
		})
		if err != nil && err != git.ErrRepositoryAlreadyExists {
			return err
		}
	}
	return nil
}

// IsGitCloned will test if the path is a git dir.
func IsGitCloned(gitPath string) (bool, error) {
	f, err := os.Stat(filepath.Join(gitPath, ".git"))
	if os.IsNotExist(err) {
		return false, nil
	}
	return err == nil && f.IsDir(), err
}

// update will fetch origin and set HEAD to origin/HEAD
// and also will create a pristine working directory by removing
// untracked files and directories.
func updateAndCleanUntracked(destinationPath string) error {
	repo, err := git.PlainOpen(destinationPath)
	if err != nil {
		return err
	}
	err = repo.Fetch(&git.FetchOptions{})
	if err != nil && err != git.NoErrAlreadyUpToDate {
		return err
	}
	wt, err := repo.Worktree()
	if err != nil {
		return err
	}
	err = wt.Pull(&git.PullOptions{})
	if err != nil && err != git.NoErrAlreadyUpToDate {
		return err
	}
	err = wt.Reset(&git.ResetOptions{
		Commit: plumbing.ZeroHash,
		Mode:   git.HardReset,
	})
	if err != nil {
		return err
	}
	return wt.Clean(&git.CleanOptions{
		Dir: true,
	})
}

// EnsureUpdated will ensure the destination path exists and is up to date.
func EnsureUpdated(destinationPath string) error {
	if err := EnsureCloned(destinationPath); err != nil {
		return err
	}
	return updateAndCleanUntracked(destinationPath)
}
