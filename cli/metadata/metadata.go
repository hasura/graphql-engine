package metadata

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"gopkg.in/yaml.v2"

	gyaml "github.com/ghodss/yaml"
	dbTypes "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
	"github.com/pkg/errors"
)

type MetadataConfig struct {
	MigrationsDirectory string
	MetadataFiles       []string
}

func New(baseDir string) *MetadataConfig {
	return &MetadataConfig{
		MigrationsDirectory: baseDir,
		MetadataFiles:       []string{filepath.Join(baseDir, "metadata.yaml"), filepath.Join(baseDir, "metadata.json")},
	}
}

func (m *MetadataConfig) Validate() error {
	return nil
}

func (m *MetadataConfig) CreateFiles() error {
	return nil
}

func (m *MetadataConfig) Build(metadata *dbTypes.Metadata) error {
	// Read metadata.yaml or metadata.json and set to metadata
	var metadataContent []byte
	for _, format := range []string{"yaml", "json"} {
		metadataPath, err := m.GetMetadataFilePath(format)
		if err != nil {
			return err
		}

		metadataContent, err = ioutil.ReadFile(metadataPath)
		if err != nil {
			if os.IsNotExist(err) {
				continue
			}
			return err
		}
		break
	}
	if metadataContent == nil {
		return errors.New("Unable to locate metadata.[yaml|json] file under migrations directory")
	}
	return gyaml.Unmarshal(metadataContent, &metadata)
}

func (m *MetadataConfig) Export(metadata dbTypes.Metadata) (dbTypes.MetadataFiles, error) {
	// create metadata.yaml file
	metaByt, err := yaml.Marshal(metadata)
	if err != nil {
		return dbTypes.MetadataFiles{}, errors.Wrap(err, "cannot marshal metadata")
	}
	metadataPath, err := m.GetMetadataFilePath("yaml")
	if err != nil {
		return dbTypes.MetadataFiles{}, errors.Wrap(err, "cannot save metadata")
	}
	return dbTypes.MetadataFiles{
		{
			Path:    metadataPath,
			Content: metaByt,
		},
	}, nil
}

func (m *MetadataConfig) GetMetadataFilePath(format string) (string, error) {
	ext := fmt.Sprintf(".%s", format)
	for _, filePath := range m.MetadataFiles {
		switch p := filepath.Ext(filePath); p {
		case ext:
			return filePath, nil
		}
	}
	return "", errors.New("unsupported file type")
}

// GetExistingMetadataFile returns the path to the default metadata file that
// also exists, json or yaml
func (m *MetadataConfig) GetExistingMetadataFile() (string, error) {
	filename := ""
	for _, format := range []string{"yaml", "json"} {
		f, err := m.GetMetadataFilePath(format)
		if err != nil {
			return "", errors.Wrap(err, "cannot get metadata file")
		}

		filename = f
		if _, err := os.Stat(filename); os.IsNotExist(err) {
			continue
		}
		break
	}

	return filename, nil
}
