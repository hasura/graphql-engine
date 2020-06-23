package hasuradb

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"

	gyaml "github.com/ghodss/yaml"
	"github.com/hasura/graphql-engine/cli/metadata/types"
	"github.com/hasura/graphql-engine/cli/migrate/database"
	"github.com/pkg/errors"
	"gopkg.in/yaml.v2"

	"github.com/oliveagle/jsonpath"
)

func (h *HasuraDB) SetMetadataPlugins(plugins types.MetadataPlugins) {
	h.config.Plugins = plugins
}

func (h *HasuraDB) EnableCheckMetadataConsistency(enabled bool) {
	h.config.enableCheckMetadataConsistency = enabled
}

func (h *HasuraDB) ExportMetadata() (map[string][]byte, error) {
	query := HasuraQuery{
		Type: "export_metadata",
		Args: HasuraArgs{},
	}

	resp, body, err := h.sendv1Query(query)
	if err != nil {
		h.logger.Debug(err)
		return nil, err
	}
	h.logger.Debug("response: ", string(body))

	if resp.StatusCode != http.StatusOK {
		return nil, NewHasuraError(body, h.config.isCMD)
	}

	var c yaml.MapSlice
	err = yaml.Unmarshal(body, &c)
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
	query := HasuraInterfaceQuery{
		Type: "clear_metadata",
		Args: HasuraArgs{},
	}

	resp, body, err := h.sendv1Query(query)
	if err != nil {
		h.logger.Debug(err)
		return err
	}
	h.logger.Debug("response: ", string(body))

	if resp.StatusCode != http.StatusOK {
		return NewHasuraError(body, h.config.isCMD)
	}
	return nil
}

// ReloadMetadata - Reload Hasura GraphQL Engine metadata on the database
func (h *HasuraDB) ReloadMetadata() error {
	query := HasuraInterfaceQuery{
		Type: "reload_metadata",
		Args: HasuraArgs{},
	}

	resp, body, err := h.sendv1Query(query)
	if err != nil {
		h.logger.Debug(err)
		return err
	}
	h.logger.Debug("response: ", string(body))

	if resp.StatusCode != http.StatusOK {
		return NewHasuraError(body, h.config.isCMD)
	}
	return nil
}

func (h *HasuraDB) GetInconsistentMetadata() (bool, []database.InconsistentMetadataInterface, error) {
	query := HasuraInterfaceQuery{
		Type: "get_inconsistent_metadata",
		Args: HasuraArgs{},
	}

	resp, body, err := h.sendv1Query(query)
	if err != nil {
		h.logger.Debug(err)
		return false, nil, err
	}
	h.logger.Debug("response: ", string(body))

	if resp.StatusCode != http.StatusOK {
		return false, nil, NewHasuraError(body, h.config.isCMD)
	}

	var inMet InconsistentMetadata
	err = json.Unmarshal(body, &inMet)
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
	query := HasuraInterfaceQuery{
		Type: "drop_inconsistent_metadata",
		Args: HasuraArgs{},
	}

	resp, body, err := h.sendv1Query(query)
	if err != nil {
		h.logger.Debug(err)
		return err
	}
	h.logger.Debug("response: ", string(body))

	if resp.StatusCode != http.StatusOK {
		return NewHasuraError(body, h.config.isCMD)
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
	var obj interface{}
	err = json.Unmarshal(jbyt, &obj)
	if err != nil {
		return err
	}
	query := HasuraInterfaceBulk{
		Type: "bulk",
		Args: []interface{}{
			HasuraInterfaceQuery{
				Type: "clear_metadata",
				Args: HasuraArgs{},
			},
			HasuraInterfaceQuery{
				Type: "replace_metadata",
				Args: obj,
			},
		},
	}
	resp, body, err := h.sendv1Query(query)
	if err != nil {
		h.logger.Debug(err)
		return err
	}
	h.logger.Debug("response: ", string(body))

	if resp.StatusCode != http.StatusOK {
		switch herror := NewHasuraError(body, h.config.isCMD).(type) {
		case HasuraError:
			if herror.Path != "" {
				jsonData, err := json.Marshal(query)
				if err != nil {
					return err
				}
				var metadataQuery interface{}
				err = json.Unmarshal(jsonData, &metadataQuery)
				if err != nil {
					return err
				}
				lookup, err := jsonpath.JsonPathLookup(metadataQuery, herror.Path)
				if err == nil {
					queryData, err := json.MarshalIndent(lookup, "", "  ")
					if err != nil {
						return err
					}
					herror.migrationQuery = "offending object: \n\r\n\r" + string(queryData)
				}
			}
			return herror
		default:
			return herror
		}
	}
	return nil
}

func (h *HasuraDB) Query(data interface{}) error {
	query := HasuraInterfaceQuery{
		Type: "bulk",
		Args: data,
	}

	resp, body, err := h.sendv1Query(query)
	if err != nil {
		h.logger.Debug(err)
		return err
	}
	h.logger.Debug("response: ", string(body))

	if resp.StatusCode != http.StatusOK {
		return NewHasuraError(body, h.config.isCMD)
	}
	return nil
}
