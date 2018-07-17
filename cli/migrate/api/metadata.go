package api

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
	"net/url"
	"strings"

	"github.com/ghodss/yaml"
	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/sirupsen/logrus"
)

func MetadataAPI(c *gin.Context) {
	// Get File url
	sourcePtr, ok := c.Get("filedir")
	if !ok {
		return
	}

	sourceURL := sourcePtr.(*url.URL)

	// Get hasuradb url
	databasePtr, ok := c.Get("dbpath")
	if !ok {
		return
	}

	// Convert to url.URL
	databaseURL := databasePtr.(*url.URL)

	// Get Logger
	loggerPtr, ok := c.Get("logger")
	if !ok {
		return
	}
	logger := loggerPtr.(*logrus.Logger)

	metadataFilePtr, ok := c.Get("metadataFile")
	if !ok {
		return
	}
	metadataFile := metadataFilePtr.(string)

	// Create new migrate
	t, err := migrate.New(sourceURL.String(), databaseURL.String(), false, logger)
	if err != nil {
		if strings.HasPrefix(err.Error(), DataAPIError) {
			c.JSON(http.StatusInternalServerError, &Response{Code: "data_api_error", Message: err.Error()})
			return
		}
		c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
		return
	}

	// Switch on request method
	switch c.Request.Method {
	case "GET":
		metaData, err := t.ExportMetadata()
		if err != nil {
			if strings.HasPrefix(err.Error(), DataAPIError) {
				c.JSON(http.StatusInternalServerError, &Response{Code: "data_api_error", Message: err.Error()})
				return
			}

			if err == migrate.ErrMigrationMode {
				c.JSON(http.StatusBadRequest, &Response{Code: "migration_mode_enabled", Message: err.Error()})
				return
			}

			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}

		queryValues := c.Request.URL.Query()
		export := queryValues.Get("export")
		if export == "true" {
			t, err := json.Marshal(metaData)
			if err != nil {
				c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
				return
			}

			data, err := yaml.JSONToYAML(t)
			if err != nil {
				c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
				return
			}

			err = ioutil.WriteFile(metadataFile, data, 0644)
			if err != nil {
				c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
				return
			}
		}
		c.JSON(http.StatusOK, &gin.H{"metadata": metaData})
	case "POST":
		var request Request

		// Bind Request body to Request struct
		if c.BindJSON(&request) != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: "Something went wrong"})
			return
		}

		err := t.Query(request.Up)
		if err != nil {
			if strings.HasPrefix(err.Error(), DataAPIError) {
				c.JSON(http.StatusInternalServerError, &Response{Code: "data_api_error", Message: err.Error()})
				return
			}

			if err == migrate.ErrMigrationMode {
				c.JSON(http.StatusBadRequest, &Response{Code: "migration_mode_enabled", Message: err.Error()})
				return
			}

			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}

		metaData, err := t.ExportMetadata()
		if err != nil {
			if strings.HasPrefix(err.Error(), DataAPIError) {
				c.JSON(http.StatusInternalServerError, &Response{Code: "data_api_error", Message: err.Error()})
				return
			}
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}

		t, err := json.Marshal(metaData)
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}

		data, err := yaml.JSONToYAML(t)
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}

		err = ioutil.WriteFile(metadataFile, data, 0644)
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}
		c.JSON(http.StatusOK, &gin.H{"message": "Success"})
	default:
		c.JSON(http.StatusMethodNotAllowed, &gin.H{"message": "Method not allowed"})
	}
}
