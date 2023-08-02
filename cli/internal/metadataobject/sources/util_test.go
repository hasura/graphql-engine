package sources

import (
	"testing"

	"github.com/stretchr/testify/require"
	"gopkg.in/yaml.v3"
)

func TestGetTableYamlFileName(t *testing.T) {
	yamlStr := `
- table:
    name: bookings
    schema: cd
- table:
    name: facilities
    schema: cd
- table:
    name: members
    dataset: cd
  object_relationship:
    - name: from_author
- table:
    - chinook
    - sqlite
    - album
    - artist
  object_relationship:
    - name: from_author
- table:
    - chinook
    - sqlite
    - artist
    - album
- table:
    - chinook123
`
	want := []string{
		"cd_bookings.yaml",
		"cd_facilities.yaml",
		"cd_members.yaml",
		"chinook_sqlite_album_artist.yaml",
		"chinook_sqlite_artist_album.yaml",
		"chinook123.yaml",
	}
	var tablesKey []struct {
		Table yaml.Node `yaml:"table"`
	}
	err := yaml.Unmarshal([]byte(yamlStr), &tablesKey)
	require.NoError(t, err)
	var filenames []string
	for _, table := range tablesKey {
		name, err := getTableYamlFileName(table.Table)
		require.NoError(t, err)
		filenames = append(filenames, name)
	}
	require.Equal(t, want, filenames)
}
