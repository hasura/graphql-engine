package util

import (
	stderrors "errors"
	"fmt"
	"io"
	"io/fs"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/spf13/afero"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

// from https://gist.github.com/r0l1/92462b38df26839a3ca324697c8cba04

// CopyFile copies the contents of the file named src to the file named
// by dst. The file will be created if it does not already exist. If the
// destination file exists, all it's contents will be replaced by the contents
// of the source file. The file mode will be copied from the source and
// the copied data is synced/flushed to stable storage.
func CopyFile(src, dst string) error {
	var op errors.Op = "util.CopyFile"
	in, err := os.Open(src)
	if err != nil {
		return errors.E(op, err)
	}
	defer in.Close()

	out, err := os.Create(dst)
	if err != nil {
		return errors.E(op, err)
	}
	defer func() {
		if e := out.Close(); e != nil {
			err = errors.E(op, e)
		}
	}()

	_, err = io.Copy(out, in)
	if err != nil {
		return errors.E(op, err)
	}

	err = out.Sync()
	if err != nil {
		return errors.E(op, err)
	}

	si, err := os.Stat(src)
	if err != nil {
		return errors.E(op, err)
	}
	err = os.Chmod(dst, si.Mode())
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

// CopyDir recursively copies a directory tree, attempting to preserve permissions.
// Source directory must exist, destination directory must *not* exist.
// Symlinks are ignored and skipped.
func CopyDir(src string, dst string) error {
	var op errors.Op = "util.CopyDir"
	src = filepath.Clean(src)
	dst = filepath.Clean(dst)

	si, err := os.Stat(src)
	if err != nil {
		return errors.E(op, err)
	}
	if !si.IsDir() {
		return errors.E(op, fmt.Errorf("source is not a directory"))
	}

	_, err = os.Stat(dst)
	if err != nil && !stderrors.Is(err, fs.ErrNotExist) {
		return errors.E(op, err)
	}
	if err == nil {
		return errors.E(op, fmt.Errorf("destination already exists"))
	}

	err = os.MkdirAll(dst, si.Mode())
	if err != nil {
		return errors.E(op, err)
	}

	entries, err := ioutil.ReadDir(src)
	if err != nil {
		return errors.E(op, err)
	}

	for _, entry := range entries {
		srcPath := filepath.Join(src, entry.Name())
		dstPath := filepath.Join(dst, entry.Name())

		if entry.IsDir() {
			err = CopyDir(srcPath, dstPath)
			if err != nil {
				return errors.E(op, err)
			}
		} else {
			// Skip symlinks.
			if entry.Mode()&os.ModeSymlink != 0 {
				continue
			}

			err = CopyFile(srcPath, dstPath)
			if err != nil {
				return errors.E(op, err)
			}
		}
	}
	return nil
}

// CopyFile but with Afero
func CopyFileAfero(fs afero.Fs, src, dst string) error {
	var op errors.Op = "util.CopyFileAfero"
	in, err := fs.Open(src)
	if err != nil {
		return errors.E(op, err)
	}
	defer in.Close()

	out, err := fs.Create(dst)
	if err != nil {
		return errors.E(op, err)
	}
	defer func() {
		if e := out.Close(); e != nil {
			err = errors.E(op, e)
		}
	}()

	_, err = io.Copy(out, in)
	if err != nil {
		return errors.E(op, err)
	}

	err = out.Sync()
	if err != nil {
		return errors.E(op, err)
	}

	si, err := fs.Stat(src)
	if err != nil {
		return errors.E(op, err)
	}
	err = fs.Chmod(dst, si.Mode())
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

// CopyDir but with afero
func CopyDirAfero(afs afero.Fs, src string, dst string) error {
	var op errors.Op = "util.CopyDirAfero"
	src = filepath.Clean(src)
	dst = filepath.Clean(dst)

	si, err := afs.Stat(src)
	if err != nil {
		return errors.E(op, err)
	}
	if !si.IsDir() {
		return errors.E(op, fmt.Errorf("source is not a directory"))
	}

	_, err = afs.Stat(dst)
	if err != nil && !stderrors.Is(err, fs.ErrNotExist) {
		return errors.E(op, err)
	}
	if err == nil {
		return errors.E(op, fmt.Errorf("destination already exists"))
	}

	err = afs.MkdirAll(dst, si.Mode())
	if err != nil {
		return errors.E(op, err)
	}

	entries, err := afero.ReadDir(afs, src)
	if err != nil {
		return errors.E(op, err)
	}

	for _, entry := range entries {
		srcPath := filepath.Join(src, entry.Name())
		dstPath := filepath.Join(dst, entry.Name())

		if entry.IsDir() {
			err = CopyDirAfero(afs, srcPath, dstPath)
			if err != nil {
				return errors.E(op, err)
			}
		} else {
			// Skip symlinks.
			if entry.Mode()&os.ModeSymlink != 0 {
				continue
			}

			err = CopyFileAfero(afs, srcPath, dstPath)
			if err != nil {
				return errors.E(op, err)
			}
		}
	}
	return nil
}
