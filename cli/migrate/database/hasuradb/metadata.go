package hasuradb

import (
	"encoding/json"
	"net/http"

	log "github.com/sirupsen/logrus"
)

func (h *HasuraDB) ExportMetadata() (interface{}, error) {
	query := HasuraQuery{
		Type: "export_metadata",
		Args: HasuraArgs{},
	}

	resp, body, err := h.sendQuery(query)
	if err != nil {
		log.Debug(err)
		return nil, err
	}
	log.Debug("response: ", string(body))

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			log.Debug(err)
			return nil, err
		}
		return nil, horror.Error(h.config.isCMD)
	}

	var hres interface{}
	err = json.Unmarshal(body, &hres)
	if err != nil {
		log.Debug(err)
		return nil, err
	}

	return hres, nil
}

func (h *HasuraDB) ResetMetadata() error {
	query := HasuraInterfaceQuery{
		Type: "clear_metadata",
		Args: HasuraArgs{},
	}

	resp, body, err := h.sendQuery(query)
	if err != nil {
		log.Debug(err)
		return err
	}
	log.Debug("response: ", string(body))

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			log.Debug(err)
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

	resp, body, err := h.sendQuery(query)
	if err != nil {
		log.Debug(err)
		return err
	}
	log.Debug("response: ", string(body))

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			log.Debug(err)
			return err
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

	resp, body, err := h.sendQuery(query)
	if err != nil {
		log.Debug(err)
		return err
	}
	log.Debug("response: ", string(body))

	var horror HasuraError
	if resp.StatusCode != http.StatusOK {
		err = json.Unmarshal(body, &horror)
		if err != nil {
			log.Debug(err)
			return err
		}
		return horror.Error(h.config.isCMD)
	}
	return nil
}
