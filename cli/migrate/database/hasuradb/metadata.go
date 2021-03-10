package hasuradb

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"

	"github.com/hasura/graphql-engine/cli/internal/hasura"

	gyaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/metadata/types"
	"github.com/hasura/graphql-engine/cli/migrate/database"
	"github.com/pkg/errors"
	"gopkg.in/yaml.v2"
)

func (h *HasuraDB) SetMetadataPlugins(plugins types.MetadataPlugins) {
	h.config.Plugins = plugins
}

func (h *HasuraDB) EnableCheckMetadataConsistency(enabled bool) {
	h.config.enableCheckMetadataConsistency = enabled
}

func (h *HasuraDB) ExportMetadata() (map[string][]byte, error) {
	resp, err := h.metadataops.ExportMetadata()
	if err != nil {
		return nil, err
	}
	var c yaml.MapSlice
	err = yaml.NewDecoder(resp).Decode(&c)
	if err != nil {
		h.logger.Debug(err)
		return nil, err
	}

	metadataFiles := make(map[string][]byte)
	for _, plg := range h.config.Plugins {
		files, err := plg.Export(c)
		if err != nil {
			return nil, errors.Wrap(err, fmt.Sprintf("cannot export %s from metadata", plg.Name()))
		}
		for fileName, content := range files {
			metadataFiles[fileName] = content
		}
	}
	return metadataFiles, nil
}

func (h *HasuraDB) ResetMetadata() error {
	_, err := h.metadataops.ClearMetadata()
	if err != nil {
		return err
	}
	return nil
}

// ReloadMetadata - Reload Hasura GraphQL Engine metadata on the database
func (h *HasuraDB) ReloadMetadata() error {
	_, err := h.metadataops.ReloadMetadata()
	if err != nil {
		return err
	}
	return nil
}

func (h *HasuraDB) GetInconsistentMetadata() (bool, []database.InconsistentMetadataInterface, error) {
	resp, err := h.metadataops.GetInconsistentMetadataReader()
	if err != nil {
		return false, nil, err
	}

	var inMet InconsistentMetadata
	err = json.NewDecoder(resp).Decode(&inMet)
	if err != nil {
		return false, nil, err
	}
	inMetInterface := make([]database.InconsistentMetadataInterface, 0)
	for _, obj := range inMet.InConsistentObjects {
		inMetInterface = append(inMetInterface, database.InconsistentMetadataInterface(obj))
	}
	return inMet.IsConsistent, inMetInterface, nil
}

func (h *HasuraDB) DropInconsistentMetadata() error {
	_, err := h.metadataops.DropInconsistentMetadata()
	if err != nil {
		return err
	}
	return nil
}

func (h *HasuraDB) BuildMetadata() (yaml.MapSlice, error) {
	var tmpMeta yaml.MapSlice
	for _, plg := range h.config.Plugins {
		err := plg.Build(&tmpMeta)
		if err != nil {
			if os.IsNotExist(errors.Cause(err)) {
				h.logger.Debugf("metadata file for %s was not found, assuming an empty file", plg.Name())
				continue
			}
			return tmpMeta, errors.Wrap(err, fmt.Sprintf("cannot build %s from metadata", plg.Name()))
		}
	}
	return tmpMeta, nil
}

func (h *HasuraDB) ApplyMetadata() error {
	tmpMeta, err := h.BuildMetadata()
	if err != nil {
		return err
	}
	yByt, err := yaml.Marshal(tmpMeta)
	if err != nil {
		return err
	}
	jbyt, err := gyaml.YAMLToJSON(yByt)
	if err != nil {
		return err
	}
	if h.v2metadataops != nil {
		var metadata interface{}
		if err := json.Unmarshal(jbyt, &metadata); err != nil {
			return err
		}
		_, err = h.v2metadataops.V2ReplaceMetadata(hasura.V2ReplaceMetadataArgs{
			AllowInconsistentMetadata: true,
			Metadata:                  metadata,
		})
		if err != nil {
			h.logger.Debug(err)
			return err
		}
	} else {
		_, err := h.metadataops.ReplaceMetadata(bytes.NewReader(jbyt))
		if err != nil {
			h.logger.Debug(err)
			return err
		}
	}
	return nil
}
