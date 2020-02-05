package api

import (
	"net/http"
	"strings"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/migrate"
)

func MetadataAPI(c *gin.Context) {
	// Get migrate instance
	migratePtr, ok := c.Get("migrate")
	if !ok {
		return
	}

	// Convert to url.URL
	t := migratePtr.(*migrate.Migrate)

	// Switch on request method
	switch c.Request.Method {
	case "GET":
		files, err := t.ExportMetadata()
		if err != nil {
			if strings.HasPrefix(err.Error(), DataAPIError) {
				c.JSON(http.StatusInternalServerError, &Response{Code: "data_api_error", Message: err.Error()})
				return
			}

			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}

		queryValues := c.Request.URL.Query()
		export := queryValues.Get("export")
		if export == "true" {
			err := t.WriteMetadata(files)
			if err != nil {
				c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
				return
			}
		}
		c.JSON(http.StatusOK, &gin.H{"metadata": "Success"})
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

		files, err := t.ExportMetadata()
		if err != nil {
			if strings.HasPrefix(err.Error(), DataAPIError) {
				c.JSON(http.StatusInternalServerError, &Response{Code: "data_api_error", Message: err.Error()})
				return
			}
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}
		err = t.WriteMetadata(files)
		if err != nil {
			if strings.HasPrefix(err.Error(), DataAPIError) {
				c.JSON(http.StatusInternalServerError, &Response{Code: "data_api_error", Message: err.Error()})
				return
			}
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}
		c.JSON(http.StatusOK, &gin.H{"message": "Success"})
	default:
		c.JSON(http.StatusMethodNotAllowed, &gin.H{"message": "Method not allowed"})
	}
}
