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

func (a *MetadataConfig) Validate() error {
	return nil
}

func (a *MetadataConfig) Build(metadata *dbTypes.Metadata) error {
	// Read metadata.yaml or metadata.json and set to metadata
	var metadataContent []byte
	for _, format := range []string{"yaml", "json"} {
		metadataPath, err := a.GetMetadataFilePath(format)
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

func (a *MetadataConfig) Export(metadata dbTypes.Metadata) error {
	// create metadata.yaml file
	metaByt, err := yaml.Marshal(metadata)
	if err != nil {
		return errors.Wrap(err, "cannot marshal metadata")
	}
	metadataPath, err := a.GetMetadataFilePath("yaml")
	if err != nil {
		return errors.Wrap(err, "cannot save metadata")
	}
	err = ioutil.WriteFile(metadataPath, metaByt, 0644)
	if err != nil {
		return errors.Wrap(err, "cannot save metadata")
	}
	return nil
}

func (a *MetadataConfig) GetMetadataFilePath(format string) (string, error) {
	ext := fmt.Sprintf(".%s", format)
	for _, filePath := range a.MetadataFiles {
		switch p := filepath.Ext(filePath); p {
		case ext:
			return filePath, nil
		}
	}
	return "", errors.New("unsupported file type")
}

// GetExistingMetadataFile returns the path to the default metadata file that
// also exists, json or yaml
func (a *MetadataConfig) GetExistingMetadataFile() (string, error) {
	filename := ""
	for _, format := range []string{"yaml", "json"} {
		f, err := a.GetMetadataFilePath(format)
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
