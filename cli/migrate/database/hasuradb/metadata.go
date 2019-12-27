package hasuradb

import (
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/migrate/database"

	"github.com/oliveagle/jsonpath"
	v2yaml "gopkg.in/yaml.v2"
)

func (h *HasuraDB) ExportMetadata() (interface{}, error) {
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

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			h.logger.Debug(err)
			return nil, fmt.Errorf("failed parsing json: %v; response from API: %s", err, string(body))
		}
		return nil, horror.Error(h.config.isCMD)
	}

	var hres v2yaml.MapSlice
	err = v2yaml.Unmarshal(body, &hres)
	if err != nil {
		h.logger.Debug(err)
		return nil, err
	}
	return hres, nil
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

func (h *HasuraDB) ApplyMetadata(data interface{}) error {
	query := HasuraInterfaceBulk{
		Type: "bulk",
		Args: []interface{}{
			HasuraInterfaceQuery{
				Type: "clear_metadata",
				Args: HasuraArgs{},
			},
			HasuraInterfaceQuery{
				Type: "replace_metadata",
				Args: data,
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

func (h *HasuraDB) Query(data []interface{}) error {
	query := HasuraInterfaceBulk{
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
