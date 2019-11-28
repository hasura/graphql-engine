package api

import (
	"net/http"
	"strings"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/migrate"
)

type SettingRequest struct {
	Name  string `json:"name"`
	Value string `json:"value"`
}

func SettingsAPI(c *gin.Context) {
	// Get migrate instance
	migratePtr, ok := c.Get("migrate")
	if !ok {
		return
	}

	t := migratePtr.(*migrate.Migrate)

	// Switch on request method
	switch c.Request.Method {
	case "GET":
		name := "migration_mode"
		setting, err := t.GetSetting(name)
		if err != nil {
			if strings.HasPrefix(err.Error(), DataAPIError) {
				c.JSON(500, &Response{Code: "data_api_error", Message: err.Error()})
				return
			}
			c.JSON(500, &Response{Code: "internal_error", Message: err.Error()})
			return
		}
		c.JSON(200, &gin.H{name: setting})
	case "PUT":
		var request SettingRequest
		// Bind Request body to Request struct
		if c.BindJSON(&request) != nil {
			c.JSON(500, &Response{Code: "internal_error", Message: "Something went wrong"})
			return
		}

		err := t.UpdateSetting(request.Name, request.Value)
		if err != nil {
			if strings.HasPrefix(err.Error(), DataAPIError) {
				c.JSON(500, &Response{Code: "data_api_error", Message: err.Error()})
				return
			}
			c.JSON(500, &Response{Code: "internal_error", Message: err.Error()})
			return
		}
		c.JSON(200, &Response{Message: "Successfuly set"})
	default:
		c.JSON(http.StatusMethodNotAllowed, &gin.H{"message": "Method not allowed"})
	}
}
