package update

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

func TestDownloadAssetHTTPError(t *testing.T) {
	for _, status := range []int{
		http.StatusNotFound,
		http.StatusForbidden,
		http.StatusTooManyRequests,
		http.StatusInternalServerError,
	} {
		t.Run(http.StatusText(status), func(t *testing.T) {
			server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				w.WriteHeader(status)
				_, _ = w.Write([]byte("not a CLI binary"))
			}))
			defer server.Close()

			dir := t.TempDir()
			const fileName = ".hasura.new"
			const original = "existing file"
			path := filepath.Join(dir, fileName)
			if err := os.WriteFile(path, []byte(original), 0o755); err != nil {
				t.Fatal(err)
			}

			url := server.URL + "/releases/download/v2.51.1/cli-hasura-linux-amd64"
			asset, err := downloadAsset(url, fileName, dir)
			if err == nil {
				t.Fatal("expected an error for a non-200 download response")
			}
			if asset != nil {
				t.Fatal("expected no asset for a failed download")
			}

			// Match the wrapping performed by ApplyUpdate and the update-cli command.
			// fmt recovers panics from Error methods, so checking only err != nil
			// would miss the nil-error regression.
			wrapped := fmt.Errorf("apply update: %w", fmt.Errorf("download asset: %w", err))
			message := wrapped.Error()
			if strings.Contains(message, "PANIC") {
				t.Fatalf("error formatting panicked: %s", message)
			}
			if !strings.Contains(message, url) {
				t.Errorf("error %q does not contain asset URL %q", message, url)
			}
			wantStatus := fmt.Sprintf("%d %s", status, http.StatusText(status))
			if !strings.Contains(message, wantStatus) {
				t.Errorf("error %q does not contain HTTP status %q", message, wantStatus)
			}
			updateErr, ok := err.(*errors.Error)
			if !ok || updateErr.Op != "update.downloadAsset" {
				t.Errorf("expected downloadAsset operation, got %#v", err)
			}

			content, readErr := os.ReadFile(path)
			if readErr != nil {
				t.Fatal(readErr)
			}
			if string(content) != original {
				t.Errorf("failed download overwrote existing file: %q", content)
			}
		})
	}
}

func TestDownloadAssetSuccess(t *testing.T) {
	const binary = "CLI binary contents\x00"
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		_, _ = w.Write([]byte(binary))
	}))
	defer server.Close()

	dir := t.TempDir()
	const fileName = ".hasura.new"
	asset, err := downloadAsset(server.URL, fileName, dir)
	if err != nil {
		t.Fatalf("downloadAsset: %v", err)
	}
	if asset == nil {
		t.Fatal("expected a downloaded asset")
	}
	wantPath := filepath.Join(dir, fileName)
	if asset.Name() != wantPath {
		t.Errorf("asset path = %q, want %q", asset.Name(), wantPath)
	}
	content, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatal(err)
	}
	if string(content) != binary {
		t.Errorf("downloaded contents = %q, want %q", content, binary)
	}
}
