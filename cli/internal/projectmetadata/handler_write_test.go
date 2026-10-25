package projectmetadata

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func dirNames(t *testing.T, dir string) []string {
	t.Helper()
	entries, err := os.ReadDir(dir)
	require.NoError(t, err)
	names := make([]string, 0, len(entries))
	for _, e := range entries {
		names = append(names, e.Name())
	}
	return names
}

func TestWriteMetadataReplacesCaseOnlyRenamedFile(t *testing.T) {
	tablesDir := filepath.Join(t.TempDir(), "databases", "default", "tables")
	require.NoError(t, os.MkdirAll(tablesDir, os.ModePerm))
	require.NoError(t, os.WriteFile(filepath.Join(tablesDir, "public_User.yaml"), []byte("old"), 0o644))
	require.NoError(t, os.WriteFile(filepath.Join(tablesDir, "public_other.yaml"), []byte("other"), 0o644))

	// export keys use forward slashes, like metadataobject.DefaultExport does
	err := (&Handler{}).WriteMetadata(map[string][]byte{
		filepath.ToSlash(filepath.Join(tablesDir, "public_user.yaml")): []byte("new"),
	})
	require.NoError(t, err)

	assert.ElementsMatch(t, []string{"public_user.yaml", "public_other.yaml"}, dirNames(t, tablesDir))
	b, err := os.ReadFile(filepath.Join(tablesDir, "public_user.yaml"))
	require.NoError(t, err)
	assert.Equal(t, "new", string(b))
}

func TestRemoveCaseVariantsKeepsFilesWrittenInTheSameBatch(t *testing.T) {
	dir := t.TempDir()
	upper := filepath.Join(dir, "public_User.yaml")
	require.NoError(t, os.WriteFile(upper, []byte("User"), 0o644))

	// two tables "User" and "user" can coexist in postgres, so a variant that
	// is part of the export must not be removed
	written := map[string]struct{}{
		upper:                                  {},
		filepath.Join(dir, "public_user.yaml"): {},
	}
	require.NoError(t, removeCaseVariants(filepath.Join(dir, "public_user.yaml"), written))

	assert.FileExists(t, upper)
}
