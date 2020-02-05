package api

import (
	"io/ioutil"
	"net/http"
	"strings"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/migrate"
	v2yaml "gopkg.in/yaml.v2"
)

func MetadataAPI(c *gin.Context) {
	// Get migrate instance
	migratePtr, ok := c.Get("migrate")
	if !ok {
		return
	}

	// Convert to url.URL
	t := migratePtr.(*migrate.Migrate)

	metadataFilePtr, ok := c.Get("metadataFile")
	if !ok {
		return
	}
	metadataFile := metadataFilePtr.(string)

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
			data, err := v2yaml.Marshal(metaData)
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

		data, err := v2yaml.Marshal(metaData)
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
