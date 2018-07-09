package api

import (
	"net/http"
	"net/url"
	"strings"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/sirupsen/logrus"
)

type SettingReqeust struct {
	Name  string `json:"name"`
	Value string `json:"value"`
}

func SettingsAPI(c *gin.Context) {
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

	// Create new migrate
	t, err := migrate.New(sourceURL.String(), databaseURL.String(), false, logger)
	if err != nil {
		if strings.HasPrefix(err.Error(), DataAPIError) {
			c.JSON(500, &Response{Code: "data_api_error", Message: err.Error()})
			return
		}
		c.JSON(500, &Response{Code: "internal_error", Message: err.Error()})
		return
	}

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
		var request SettingReqeust
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
