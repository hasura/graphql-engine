package download

import (
	"io"
	"net/http"
	"os"

	"github.com/pkg/errors"
)

// Fetcher is used to get files from a URI.
type Fetcher interface {
	// Get gets the file and returns an stream to read the file.
	Get(uri string) (io.ReadCloser, error)
}

var _ Fetcher = HTTPFetcher{}

// HTTPFetcher is used to get a file from a http:// or https:// schema path.
type HTTPFetcher struct{}

// Get gets the file and returns an stream to read the file.
func (HTTPFetcher) Get(uri string) (io.ReadCloser, error) {
	resp, err := http.Get(uri)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to download %q", uri)
	}
	return resp.Body, nil
}

var _ Fetcher = fileFetcher{}

type fileFetcher struct{ f string }

func (f fileFetcher) Get(_ string) (io.ReadCloser, error) {
	file, err := os.Open(f.f)
	return file, errors.Wrapf(err, "failed to open archive file %q for reading", f.f)
}

// NewFileFetcher returns a local file reader.
func NewFileFetcher(path string) Fetcher { return fileFetcher{f: path} }
