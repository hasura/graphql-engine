package hasuradb

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"

	"github.com/hasura/graphql-engine/cli/migrate/database"
	"github.com/hasura/graphql-engine/cli/migrate/database/hasuradb/types"
	"github.com/pkg/errors"
	"gopkg.in/yaml.v2"

	"github.com/oliveagle/jsonpath"
)

func (h *HasuraDB) SetMetadataPlugins(plugins interface{}) {
	h.config.Plugins = plugins.(types.MetadataPlugins)
}

func (h *HasuraDB) ExportMetadata() error {
	query := HasuraQuery{
		Type: "export_metadata",
		Args: HasuraArgs{},
	}

	resp, body, err := h.sendv1Query(query)
	if err != nil {
		h.logger.Debug(err)
		return err
	}
	h.logger.Debug("response: ", string(body))

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			h.logger.Debug(err)
			return fmt.Errorf("failed parsing json: %v; response from API: %s", err, string(body))
		}
		return horror.Error(h.config.isCMD)
	}

	var c yaml.MapSlice
	err = yaml.Unmarshal(body, &c)
	if err != nil {
		h.logger.Debug(err)
		return err
	}

	var metadataFiles types.MetadataFiles
	for plgName, plg := range h.config.Plugins {
		files, err := plg.Export(c)
		if err != nil {
			return errors.Wrap(err, fmt.Sprintf("cannot export %s from metadata", plgName))
		}
		metadataFiles = append(metadataFiles, files...)
	}

	// create files
	for _, file := range metadataFiles {
		err = ioutil.WriteFile(file.Path, file.Content, 0644)
		if err != nil {
			return errors.Wrap(err, "creating metadata file failed")
		}
	}
	return nil
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

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			h.logger.Debug(err)
			return fmt.Errorf("failed parsing json: %v; response from API: %s", err, string(body))
		}
		return horror.Error(h.config.isCMD)
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

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			h.logger.Debug(err)
			return fmt.Errorf("failed parsing json: %v; response from API: %s", err, string(body))
		}
		return horror.Error(h.config.isCMD)
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

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			h.logger.Debug(err)
			return false, nil, err
		}
		return false, nil, horror.Error(h.config.isCMD)
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

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			h.logger.Debug(err)
			return err
		}
		return horror.Error(h.config.isCMD)
	}
	return nil
}

func (h *HasuraDB) ApplyMetadata() error {
	var tmpMeta types.Metadata
	for plgName, plg := range h.config.Plugins {
		err := plg.Build(&tmpMeta)
		if err != nil {
			return errors.Wrap(err, fmt.Sprintf("cannot build %s from metadata", plgName))
		}
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
				Args: tmpMeta,
			},
		},
	}
	resp, body, err := h.sendv1Query(query)
	if err != nil {
		h.logger.Debug(err)
		return err
	}
	h.logger.Debug("response: ", string(body))

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			h.logger.Debug(err)
			return fmt.Errorf("failed parsing json: %v; response from API: %s", err, string(body))
		}

		if horror.Path != "" {
			jsonData, err := json.Marshal(query)
			if err != nil {
				return err
			}
			var metadataQuery interface{}
			err = json.Unmarshal(jsonData, &metadataQuery)
			if err != nil {
				return err
			}
			lookup, err := jsonpath.JsonPathLookup(metadataQuery, horror.Path)
			if err == nil {
				queryData, err := json.MarshalIndent(lookup, "", "  ")
				if err != nil {
					return err
				}
				horror.migrationQuery = "offending object: \n\r\n\r" + string(queryData)
			}
		}
		return horror.Error(h.config.isCMD)
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

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			h.logger.Debug(err)
			return fmt.Errorf("failed parsing json: %v; response from API: %s", err, string(body))
		}
		return horror.Error(h.config.isCMD)
	}
	return nil
}
