package util

import (
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

func FSCheckIfDirPathExists(path string) error {
	var op errors.Op = "util.FSCheckIfDirPathExists"
	stat, err := os.Lstat(path)
	if err != nil {
		return errors.E(op, err)
	}
	if !stat.IsDir() {
		err = errors.E(op, "no such directory")
	}
	return err
}

func FSCopyFile(src, dst string) error {
	var op errors.Op = "util.FSCopyFile"
	var err error
	var srcfd *os.File
	var dstfd *os.File
	var srcinfo os.FileInfo

	if srcfd, err = os.Open(src); err != nil {
		return errors.E(op, err)
	}
	defer srcfd.Close()

	if dstfd, err = os.Create(dst); err != nil {
		return errors.E(op, err)
	}
	defer dstfd.Close()

	if _, err = io.Copy(dstfd, srcfd); err != nil {
		return errors.E(op, err)
	}
	if srcinfo, err = os.Stat(src); err != nil {
		return errors.E(op, err)
	}
	err = os.Chmod(dst, srcinfo.Mode())
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}

func FSCopyDir(src string, dst string) error {
	var op errors.Op = "util.FSCopyDir"
	var err error
	var fds []os.FileInfo
	var srcinfo os.FileInfo

	if srcinfo, err = os.Stat(src); err != nil {
		return errors.E(op, err)
	}

	if err = os.MkdirAll(dst, srcinfo.Mode()); err != nil {
		return errors.E(op, err)
	}

	if fds, err = ioutil.ReadDir(src); err != nil {
		return errors.E(op, err)
	}
	for _, fd := range fds {
		srcfp := path.Join(src, fd.Name())
		dstfp := path.Join(dst, fd.Name())

		if fd.IsDir() {
			if err = FSCopyDir(srcfp, dstfp); err != nil {
				fmt.Println(err)
			}
		} else {
			if err = FSCopyFile(srcfp, dstfp); err != nil {
				fmt.Println(err)
			}
		}
	}
	return nil
}
