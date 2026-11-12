package projectmetadata

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/spf13/afero"
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

func TestWriteMetadataRenamesCaseOnlyRenamedDirectory(t *testing.T) {
	metadataDir := filepath.Join(t.TempDir(), "metadata")
	oldTablesDir := filepath.Join(metadataDir, "databases", "Default", "tables")
	require.NoError(t, os.MkdirAll(oldTablesDir, os.ModePerm))
	require.NoError(t, os.WriteFile(filepath.Join(oldTablesDir, "public_User.yaml"), []byte("old"), 0o644))
	require.NoError(t, os.WriteFile(filepath.Join(oldTablesDir, "public_other.yaml"), []byte("other"), 0o644))

	err := (&Handler{}).WriteMetadata(map[string][]byte{
		filepath.ToSlash(filepath.Join(metadataDir, "actions.yaml")):                                       []byte("actions"),
		filepath.ToSlash(filepath.Join(metadataDir, "databases", "databases.yaml")):                        []byte("databases"),
		filepath.ToSlash(filepath.Join(metadataDir, "databases", "default", "tables", "public_user.yaml")): []byte("new"),
	})
	require.NoError(t, err)

	assert.ElementsMatch(t, []string{"databases.yaml", "default"}, dirNames(t, filepath.Join(metadataDir, "databases")))
	tablesDir := filepath.Join(metadataDir, "databases", "default", "tables")
	assert.ElementsMatch(t, []string{"public_user.yaml", "public_other.yaml"}, dirNames(t, tablesDir))
}

func TestWriteMetadataDoesNotRenameAboveTheExportedDirectory(t *testing.T) {
	projectDir := filepath.Join(t.TempDir(), "Project")
	require.NoError(t, os.MkdirAll(filepath.Join(projectDir, "metadata"), os.ModePerm))

	// the common directory of the export is project/metadata, so the
	// differently cased Project directory must be kept as is
	err := (&Handler{}).WriteMetadata(map[string][]byte{
		filepath.ToSlash(filepath.Join(filepath.Dir(projectDir), "project", "metadata", "actions.yaml")):                []byte("actions"),
		filepath.ToSlash(filepath.Join(filepath.Dir(projectDir), "project", "metadata", "databases", "databases.yaml")): []byte("databases"),
	})
	require.NoError(t, err)

	assert.Contains(t, dirNames(t, filepath.Dir(projectDir)), "Project")
}

func TestCaseVariantsWrittenInTheSameBatchAreKept(t *testing.T) {
	// two tables "User" and "user" (or sources "Default" and "default") can
	// coexist in postgres, so a variant that is part of the export must be
	// kept. A memory file system is case sensitive on every OS.
	fs := afero.NewMemMapFs()
	tablesDir := filepath.Join("metadata", "databases", "Default", "tables")
	require.NoError(t, fs.MkdirAll(tablesDir, os.ModePerm))
	upper := filepath.Join(tablesDir, "public_User.yaml")
	lower := filepath.Join(tablesDir, "public_user.yaml")
	require.NoError(t, afero.WriteFile(fs, upper, []byte("User"), 0o644))

	written := map[string]struct{}{
		upper: {},
		lower: {},
		filepath.Join("metadata", "databases", "Default"): {},
		filepath.Join("metadata", "databases", "default"): {},
	}
	require.NoError(t, renameCaseVariantDir(fs, filepath.Join("metadata", "databases", "default"), written))
	require.NoError(t, removeCaseVariantFiles(fs, tablesDir, []string{lower}, written))

	exists, err := afero.Exists(fs, upper)
	require.NoError(t, err)
	assert.True(t, exists)
}
