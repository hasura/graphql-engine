package plugins

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/tree/master/internal
*/

import (
	stderrors "errors"
	"fmt"
	"io"
	"io/fs"
	"io/ioutil"
	"os"
	"path/filepath"
	"syscall"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

type move struct {
	from, to string
}

func findMoveTargets(fromDir, toDir string, fo FileOperation) ([]move, error) {
	var op errors.Op = "plugins.findMoveTargets"
	if fo.To != filepath.Clean(fo.To) {
		return nil, errors.E(op, fmt.Errorf("the provided path is not clean, %q should be %q", fo.To, filepath.Clean(fo.To)))
	}
	fromDir, err := filepath.Abs(fromDir)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("could not get the relative path for the move src: %w", err))
	}

	if m, ok, err := getDirectMove(fromDir, toDir, fo); err != nil {
		return nil, errors.E(op, fmt.Errorf("failed to detect single move operation: %w", err))
	} else if ok {
		return []move{m}, nil
	}

	newDir, err := filepath.Abs(filepath.Join(filepath.FromSlash(toDir), filepath.FromSlash(fo.To)))
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("could not get the relative path for the move dst: %w", err))
	}

	gl, err := filepath.Glob(filepath.Join(filepath.FromSlash(fromDir), filepath.FromSlash(fo.From)))
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("could not get files using a glob string: %w", err))
	}
	if len(gl) == 0 {
		return nil, errors.E(op, fmt.Errorf("no files in the plugin archive matched the glob pattern=%s", fo.From))
	}

	moves := make([]move, 0, len(gl))
	for _, v := range gl {
		newPath := filepath.Join(newDir, filepath.Base(filepath.FromSlash(v)))
		// Check secure path
		m := move{from: v, to: newPath}
		if !isMoveAllowed(fromDir, toDir, m) {
			return nil, errors.E(op, fmt.Errorf("can't move, move target %v is not a subpath from=%q, to=%q", m, fromDir, toDir))
		}
		moves = append(moves, m)
	}
	return moves, nil
}

func getDirectMove(fromDir, toDir string, fo FileOperation) (move, bool, error) {
	var op errors.Op = "plugins.getDirectMove"
	var m move
	fromDir, err := filepath.Abs(fromDir)
	if err != nil {
		return m, false, errors.E(op, fmt.Errorf("could not get the relative path for the move src: %w", err))
	}

	toDir, err = filepath.Abs(toDir)
	if err != nil {
		return m, false, errors.E(op, fmt.Errorf("could not get the relative path for the move src: %w", err))
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
		return m, false, errors.E(op, fmt.Errorf("could not get the relative path for the move dst: %w", err))
	}

	// Check sane path
	m = move{from: fromFilePath, to: toFilePath}
	if !isMoveAllowed(fromDir, toDir, m) {
		return move{}, false, errors.E(op, fmt.Errorf("can't move, move target %v is out of bounds from=%q, to=%q", m, fromDir, toDir))
	}

	return m, true, nil
}

func isMoveAllowed(fromBase, toBase string, m move) bool {
	_, okFrom := IsSubPath(fromBase, m.from)
	_, okTo := IsSubPath(toBase, m.to)
	return okFrom && okTo
}

func moveFiles(fromDir, toDir string, fo FileOperation) error {
	var op errors.Op = "plugins.moveFiles"
	moves, err := findMoveTargets(fromDir, toDir, fo)
	if err != nil {
		return errors.E(op, fmt.Errorf("could not find move targets: %w", err))
	}

	for _, m := range moves {
		if err := os.MkdirAll(filepath.Dir(m.to), 0755); err != nil {
			return errors.E(op, fmt.Errorf("failed to create move path %q: %w", filepath.Dir(m.to), err))
		}

		if err = renameOrCopy(m.from, m.to); err != nil {
			return errors.E(op, fmt.Errorf("could not rename/copy file from %q to %q: %w", m.from, m.to, err))
		}
	}
	return nil
}

func moveAllFiles(fromDir, toDir string, fos []FileOperation) error {
	var op errors.Op = "plugins.moveAllFiles"
	for _, fo := range fos {
		if err := moveFiles(fromDir, toDir, fo); err != nil {
			return errors.E(op, fmt.Errorf("failed moving files: %w", err))
		}
	}
	return nil
}

// moveToInstallDir moves plugins from srcDir to dstDir (created in this method) with given FileOperation.
func moveToInstallDir(srcDir, installDir string, fos []FileOperation) error {
	var op errors.Op = "plugins.moveToInstallDir"
	installationDir := filepath.Dir(installDir)
	if err := os.MkdirAll(installationDir, 0755); err != nil {
		return errors.E(op, fmt.Errorf("error creating directory at %q: %w", installationDir, err))
	}

	tmp, err := ioutil.TempDir("", "hasura-temp-move")
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to find a temporary director: %w", err))
	}
	defer os.RemoveAll(tmp)

	if err = moveAllFiles(srcDir, tmp, fos); err != nil {
		return errors.E(op, fmt.Errorf("failed to move files: %w", err))
	}

	if err = renameOrCopy(tmp, installDir); err != nil {
		defer func() {
			os.Remove(installDir)
		}()
		return errors.E(op, fmt.Errorf("could not rename/copy directory %q to %q: %w", tmp, installDir, err))
	}
	return nil
}

// renameOrCopy will try to rename a dir or file. If rename is not supported, a manual copy will be performed.
// Existing files at "to" will be deleted.
func renameOrCopy(from, to string) error {
	var op errors.Op = "plugins.renameOrCopy"
	fi, err := os.Stat(to)
	if err != nil && !stderrors.Is(err, fs.ErrNotExist) {
		return errors.E(op, fmt.Errorf("error checking move target dir %q: %w", to, err))
	}
	if fi != nil && fi.IsDir() {
		if err := os.RemoveAll(to); err != nil {
			return errors.E(op, fmt.Errorf("error cleaning up dir %q: %w", to, err))
		}
	}

	err = os.Rename(from, to)
	// Fallback for invalid cross-device link (errno:18).
	if isCrossDeviceRenameErr(err) {
		copyErr := copyTree(from, to)
		if copyErr != nil {
			return errors.E(op, fmt.Errorf("failed to copy directory tree as a fallback: %w", copyErr))
		}
		return nil
	}
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

// copyTree copies files or directories, recursively.
func copyTree(from, to string) (err error) {
	var op errors.Op = "plugins.copyTree"
	err = filepath.Walk(from, func(path string, info os.FileInfo, err error) error {
		var op errors.Op = "plugins.copyTree.Walk"
		if err != nil {
			return errors.E(op, err)
		}
		newPath, _ := ReplaceBase(path, from, to)
		if info.IsDir() {
			err = os.MkdirAll(newPath, info.Mode())
		} else {
			err = copyFile(path, newPath, info.Mode())
		}
		return errors.E(op, err)
	})
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func copyFile(source, dst string, mode os.FileMode) (err error) {
	var op errors.Op = "plugins.copyFile"
	sf, err := os.Open(source)
	if err != nil {
		return errors.E(op, err)
	}
	defer sf.Close()

	df, err := os.Create(dst)
	if err != nil {
		return errors.E(op, err)
	}
	defer df.Close()

	_, err = io.Copy(df, sf)
	if err != nil {
		return errors.E(op, err)
	}
	err = os.Chmod(dst, mode)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

// isCrossDeviceRenameErr determines if a os.Rename error is due to cross-fs/drive/volume copying.
func isCrossDeviceRenameErr(err error) bool {
	var le *os.LinkError
	if !stderrors.As(err, &le) {
		return false
	}
	errno, ok := le.Err.(syscall.Errno)
	if !ok {
		return false
	}
	return (IsWindows() && errno == 17) || // syscall.ERROR_NOT_SAME_DEVICE
		(!IsWindows() && errno == 18) // syscall.EXDEV
}
