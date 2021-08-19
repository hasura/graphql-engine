package api

import (
	"net/http"
	"strings"

	"github.com/hasura/graphql-engine/cli/v2/internal/projectmetadata"

	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/v2/migrate"
)

func MetadataAPI(c *gin.Context) {
	// Get migrate instance
	ecPtr, ok := c.Get("ec")
	if !ok {
		return
	}

	// Convert to url.URL
	ec, ok := ecPtr.(*cli.ExecutionContext)
	if !ok {
		c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: "cannot get execution context"})
		return
	}
	t, err := migrate.NewMigrate(ec, false, "", hasura.SourceKindPG)
	if err != nil {
		c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
		return
	}
	mdHandler := projectmetadata.NewHandlerFromEC(ec)
	// Switch on request method
	switch c.Request.Method {
	case "GET":
		var files map[string][]byte
		files, err = mdHandler.ExportMetadata()
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
			err := mdHandler.WriteMetadata(files)
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
		var files map[string][]byte
		files, err = mdHandler.ExportMetadata()
		if err != nil {
			if strings.HasPrefix(err.Error(), DataAPIError) {
				c.JSON(http.StatusInternalServerError, &Response{Code: "data_api_error", Message: err.Error()})
				return
			}
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}
		err = mdHandler.WriteMetadata(files)
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
