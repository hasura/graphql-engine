// Copyright 2019 The Kubernetes Authors.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package download

import (
	"archive/tar"
	"archive/zip"
	"bytes"
	"compress/gzip"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"path/filepath"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

// download gets a file from the internet in memory and writes it content
// to a Verifier.
func download(url string, verifier Verifier, fetcher Fetcher) (io.ReaderAt, int64, error) {
	var op errors.Op = "download.download"
	body, err := fetcher.Get(url)
	if err != nil {
		return nil, 0, errors.E(op, fmt.Errorf("failed to obtain plugin archive: %w", err))
	}
	defer body.Close()

	data, err := ioutil.ReadAll(io.TeeReader(body, verifier))
	if err != nil {
		return nil, 0, errors.E(op, fmt.Errorf("could not read archive: %w", err))
	}

	err = verifier.Verify()
	if err != nil {
		return bytes.NewReader(data), int64(len(data)), errors.E(op, err)
	}
	return bytes.NewReader(data), int64(len(data)), nil
}

// extractZIP extracts a zip file into the target directory.
func extractZIP(targetDir, fileName string, read io.ReaderAt, size int64) error {
	var op errors.Op = "download.extractZIP"
	zipReader, err := zip.NewReader(read, size)
	if err != nil {
		return errors.E(op, err)
	}

	for _, f := range zipReader.File {
		if err := suspiciousPath(f.Name); err != nil {
			return errors.E(op, err)
		}

		path := filepath.Join(targetDir, filepath.FromSlash(f.Name))
		if f.FileInfo().IsDir() {
			if err := os.MkdirAll(path, f.Mode()); err != nil {
				return errors.E(op, fmt.Errorf("can't create directory tree: %w", err))
			}
			continue
		}

		src, err := f.Open()
		if err != nil {
			return errors.E(op, fmt.Errorf("could not open inflating zip file: %w", err))
		}

		dst, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, f.Mode())
		if err != nil {
			src.Close()
			return errors.E(op, fmt.Errorf("can't create file in zip destination dir: %w", err))
		}
		closeAll := func() {
			src.Close()
			dst.Close()
		}

		if _, err := io.Copy(dst, src); err != nil {
			closeAll()
			return errors.E(op, fmt.Errorf("can't copy content to zip destination file: %w", err))
		}
		closeAll()
	}

	return nil
}

// extractTARGZ extracts a gzipped tar file into the target directory.
func extractTARGZ(targetDir, fileName string, at io.ReaderAt, size int64) error {
	var op errors.Op = "download.extractTARGZ"
	in := io.NewSectionReader(at, 0, size)

	gzr, err := gzip.NewReader(in)
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to create gzip reader: %w", err))
	}
	defer gzr.Close()

	tr := tar.NewReader(gzr)
	for {
		hdr, err := tr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return errors.E(op, fmt.Errorf("tar extraction error: %w", err))
		}
		// see https://golang.org/cl/78355 for handling pax_global_header
		if hdr.Name == "pax_global_header" {
			continue
		}

		if err := suspiciousPath(hdr.Name); err != nil {
			return errors.E(op, err)
		}

		path := filepath.Join(targetDir, filepath.FromSlash(hdr.Name))
		switch hdr.Typeflag {
		case tar.TypeDir:
			if err := os.MkdirAll(path, os.FileMode(hdr.Mode)); err != nil {
				return errors.E(op, fmt.Errorf("failed to create directory from tar: %w", err))
			}
		case tar.TypeReg:
			dir := filepath.Dir(path)
			if err := os.MkdirAll(dir, 0755); err != nil {
				return errors.E(op, fmt.Errorf("failed to create directory for tar: %w", err))
			}
			f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY, os.FileMode(hdr.Mode))
			if err != nil {
				return errors.E(op, fmt.Errorf("failed to create file %q: %w", path, err))
			}

			if _, err := io.Copy(f, tr); err != nil {
				f.Close()
				return errors.E(op, fmt.Errorf("failed to copy %q from tar into file: %w", hdr.Name, err))
			}
			f.Close()
		default:
			return errors.E(op, fmt.Errorf("unable to handle file type %d for %q in tar", hdr.Typeflag, hdr.Name))
		}
	}
	return nil
}

func extractOctetStream(targetDir, fileName string, at io.ReaderAt, size int64) error {
	var op errors.Op = "download.extractOctetStream"
	in := io.NewSectionReader(at, 0, size)
	path := filepath.Join(targetDir, fileName)

	if err := os.MkdirAll(targetDir, 0755); err != nil {
		return errors.E(op, fmt.Errorf("failed to create directory for tar: %w", err))
	}

	f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY, os.FileMode(os.ModePerm))
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to create file %q: %w", path, err))
	}
	if _, err := io.Copy(f, in); err != nil {
		f.Close()
		return errors.E(op, fmt.Errorf("failed to copy %q into file: %w", fileName, err))
	}
	f.Close()
	return nil
}

func suspiciousPath(path string) error {
	var op errors.Op = "download.suspiciousPath"
	if strings.Contains(path, "..") {
		return errors.E(op, fmt.Errorf("refusing to unpack archive with suspicious entry %q", path))
	}

	if strings.HasPrefix(path, `/`) || strings.HasPrefix(path, `\`) {
		return errors.E(op, fmt.Errorf("refusing to unpack archive with absolute entry %q", path))
	}

	return nil
}

func detectMIMEType(at io.ReaderAt) (string, error) {
	var op errors.Op = "download.detectMIMEType"
	buf := make([]byte, 512)
	n, err := at.ReadAt(buf, 0)
	if err != nil && err != io.EOF {
		return "", errors.E(op, fmt.Errorf("failed to read first 512 bytes: %w", err))
	}
	// if n < 512 {
	//klog.V(5).Infof("Did only read %d of 512 bytes to determine the file type", n)
	// }

	// Cut off mime extra info beginning with ';' i.e:
	// "text/plain; charset=utf-8" should result in "text/plain".
	return strings.Split(http.DetectContentType(buf[:n]), ";")[0], nil
}

type extractor func(targetDir, fileName string, read io.ReaderAt, size int64) error

var defaultExtractors = map[string]extractor{
	"application/zip":                 extractZIP,
	"application/x-gzip":              extractTARGZ,
	"application/octet-stream":        extractOctetStream,
	"application/x-ms-dos-executable": extractOctetStream,
}

func extractArchive(dst, fileName string, at io.ReaderAt, size int64) error {
	var op errors.Op = "download.extractArchive"
	t, err := detectMIMEType(at)
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to determine content type: %w", err))
	}
	exf, ok := defaultExtractors[t]
	if !ok {
		return errors.E(op, fmt.Errorf("mime type %q for archive file is not a supported archive format", t))
	}
	err = exf(dst, fileName, at, size)
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to extract file: %w", err))
	}
	return nil
}

// Downloader is responsible for fetching, verifying and extracting a binary.
type Downloader struct {
	verifier Verifier
	fetcher  Fetcher
}

// NewDownloader builds a new Downloader.
func NewDownloader(v Verifier, f Fetcher) Downloader {
	return Downloader{
		verifier: v,
		fetcher:  f,
	}
}

// Get pulls the uri and verifies it. On success, the download gets extracted
// into dst.
func (d Downloader) Get(uri, dst string) error {
	var op errors.Op = "download.Downloader.Get"
	body, size, err := download(uri, d.verifier, d.fetcher)
	if err != nil {
		return errors.E(op, err)
	}
	fileName := filepath.Base(uri)
	err = extractArchive(dst, fileName, body, size)
	if err != nil {
		return errors.E(op, err)
	}
	return nil
}
