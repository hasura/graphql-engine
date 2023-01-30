package util

import (
	"archive/zip"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

// from  https://gist.github.com/svett/424e6784facc0ba907ae

// Unzip unzips the archive to target. Both archive and target should be paths
// in the filesystem. target is created if it doesn't exist already.
func Unzip(archive, target string) error {
	var op errors.Op = "util.Unzip"
	reader, err := zip.OpenReader(archive)
	if err != nil {
		return errors.E(op, err)
	}

	if err := os.MkdirAll(target, 0755); err != nil {
		return errors.E(op, err)
	}

	for _, file := range reader.File {
		path := filepath.Join(target, file.Name)
		if file.FileInfo().IsDir() {
			if err = os.MkdirAll(path, file.Mode()); err != nil {
				return errors.E(op, fmt.Errorf("error while creating directory and it's parent directories: %w", err))
			}
			continue
		}

		fileReader, err := file.Open()
		if err != nil {

			if fileReader != nil {
				fileReader.Close()
			}

			return errors.E(op, err)
		}

		targetFile, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, file.Mode())
		if err != nil {
			fileReader.Close()

			if targetFile != nil {
				targetFile.Close()
			}

			return errors.E(op, err)
		}

		if _, err := io.Copy(targetFile, fileReader); err != nil {
			fileReader.Close()
			targetFile.Close()

			return errors.E(op, err)
		}

		fileReader.Close()
		targetFile.Close()
	}

	return nil
}
