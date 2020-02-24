package plugins

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/tree/master/internal
*/

import (
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"syscall"

	"github.com/pkg/errors"
)

type move struct {
	from, to string
}

func findMoveTargets(fromDir, toDir string, fo FileOperation) ([]move, error) {
	if fo.To != filepath.Clean(fo.To) {
		return nil, errors.Errorf("the provided path is not clean, %q should be %q", fo.To, filepath.Clean(fo.To))
	}
	fromDir, err := filepath.Abs(fromDir)
	if err != nil {
		return nil, errors.Wrap(err, "could not get the relative path for the move src")
	}

	if m, ok, err := getDirectMove(fromDir, toDir, fo); err != nil {
		return nil, errors.Wrap(err, "failed to detect single move operation")
	} else if ok {
		return []move{m}, nil
	}

	newDir, err := filepath.Abs(filepath.Join(filepath.FromSlash(toDir), filepath.FromSlash(fo.To)))
	if err != nil {
		return nil, errors.Wrap(err, "could not get the relative path for the move dst")
	}

	gl, err := filepath.Glob(filepath.Join(filepath.FromSlash(fromDir), filepath.FromSlash(fo.From)))
	if err != nil {
		return nil, errors.Wrap(err, "could not get files using a glob string")
	}
	if len(gl) == 0 {
		return nil, errors.Errorf("no files in the plugin archive matched the glob pattern=%s", fo.From)
	}

	moves := make([]move, 0, len(gl))
	for _, v := range gl {
		newPath := filepath.Join(newDir, filepath.Base(filepath.FromSlash(v)))
		// Check secure path
		m := move{from: v, to: newPath}
		if !isMoveAllowed(fromDir, toDir, m) {
			return nil, errors.Errorf("can't move, move target %v is not a subpath from=%q, to=%q", m, fromDir, toDir)
		}
		moves = append(moves, m)
	}
	return moves, nil
}

func getDirectMove(fromDir, toDir string, fo FileOperation) (move, bool, error) {
	var m move
	fromDir, err := filepath.Abs(fromDir)
	if err != nil {
		return m, false, errors.Wrap(err, "could not get the relative path for the move src")
	}

	toDir, err = filepath.Abs(toDir)
	if err != nil {
		return m, false, errors.Wrap(err, "could not get the relative path for the move src")
	}

	// Check is direct file (not a Glob)
	fromFilePath := filepath.Clean(filepath.Join(fromDir, fo.From))
	_, err = os.Stat(fromFilePath)
	if err != nil {
		return m, false, nil
	}

	// If target is empty use old file name.
	if filepath.Clean(fo.To) == "." {
		fo.To = filepath.Base(fromFilePath)
	}

	// Build new file name
	toFilePath, err := filepath.Abs(filepath.Join(filepath.FromSlash(toDir), filepath.FromSlash(fo.To)))
	if err != nil {
		return m, false, errors.Wrap(err, "could not get the relative path for the move dst")
	}

	// Check sane path
	m = move{from: fromFilePath, to: toFilePath}
	if !isMoveAllowed(fromDir, toDir, m) {
		return move{}, false, errors.Errorf("can't move, move target %v is out of bounds from=%q, to=%q", m, fromDir, toDir)
	}

	return m, true, nil
}

func isMoveAllowed(fromBase, toBase string, m move) bool {
	_, okFrom := IsSubPath(fromBase, m.from)
	_, okTo := IsSubPath(toBase, m.to)
	return okFrom && okTo
}

func moveFiles(fromDir, toDir string, fo FileOperation) error {
	moves, err := findMoveTargets(fromDir, toDir, fo)
	if err != nil {
		return errors.Wrap(err, "could not find move targets")
	}

	for _, m := range moves {
		if err := os.MkdirAll(filepath.Dir(m.to), 0755); err != nil {
			return errors.Wrapf(err, "failed to create move path %q", filepath.Dir(m.to))
		}

		if err = renameOrCopy(m.from, m.to); err != nil {
			return errors.Wrapf(err, "could not rename/copy file from %q to %q", m.from, m.to)
		}
	}
	return nil
}

func moveAllFiles(fromDir, toDir string, fos []FileOperation) error {
	for _, fo := range fos {
		if err := moveFiles(fromDir, toDir, fo); err != nil {
			return errors.Wrap(err, "failed moving files")
		}
	}
	return nil
}

// moveToInstallDir moves plugins from srcDir to dstDir (created in this method) with given FileOperation.
func moveToInstallDir(srcDir, installDir string, fos []FileOperation) error {
	installationDir := filepath.Dir(installDir)
	if err := os.MkdirAll(installationDir, 0755); err != nil {
		return errors.Wrapf(err, "error creating directory at %q", installationDir)
	}

	tmp, err := ioutil.TempDir("", "hasura-temp-move")
	if err != nil {
		return errors.Wrap(err, "failed to find a temporary director")
	}
	defer os.RemoveAll(tmp)

	if err = moveAllFiles(srcDir, tmp, fos); err != nil {
		return errors.Wrap(err, "failed to move files")
	}

	if err = renameOrCopy(tmp, installDir); err != nil {
		defer func() {
			os.Remove(installDir)
		}()
		return errors.Wrapf(err, "could not rename/copy directory %q to %q", tmp, installDir)
	}
	return nil
}

// renameOrCopy will try to rename a dir or file. If rename is not supported, a manual copy will be performed.
// Existing files at "to" will be deleted.
func renameOrCopy(from, to string) error {
	fi, err := os.Stat(to)
	if err != nil && !os.IsNotExist(err) {
		return errors.Wrapf(err, "error checking move target dir %q", to)
	}
	if fi != nil && fi.IsDir() {
		if err := os.RemoveAll(to); err != nil {
			return errors.Wrapf(err, "error cleaning up dir %q", to)
		}
	}

	err = os.Rename(from, to)
	// Fallback for invalid cross-device link (errno:18).
	if isCrossDeviceRenameErr(err) {
		return errors.Wrap(copyTree(from, to), "failed to copy directory tree as a fallback")
	}
	return err
}

// copyTree copies files or directories, recursively.
func copyTree(from, to string) (err error) {
	return filepath.Walk(from, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		newPath, _ := ReplaceBase(path, from, to)
		if info.IsDir() {
			err = os.MkdirAll(newPath, info.Mode())
		} else {
			err = copyFile(path, newPath, info.Mode())
		}
		return err
	})
}

func copyFile(source, dst string, mode os.FileMode) (err error) {
	sf, err := os.Open(source)
	if err != nil {
		return err
	}
	defer sf.Close()

	df, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer df.Close()

	_, err = io.Copy(df, sf)
	if err != nil {
		return err
	}
	return os.Chmod(dst, mode)
}

// isCrossDeviceRenameErr determines if a os.Rename error is due to cross-fs/drive/volume copying.
func isCrossDeviceRenameErr(err error) bool {
	le, ok := err.(*os.LinkError)
	if !ok {
		return false
	}
	errno, ok := le.Err.(syscall.Errno)
	if !ok {
		return false
	}
	return (IsWindows() && errno == 17) || // syscall.ERROR_NOT_SAME_DEVICE
		(!IsWindows() && errno == 18) // syscall.EXDEV
}
