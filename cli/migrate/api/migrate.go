package api

import (
	"fmt"
	"net/http"
	"net/url"
	"strings"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/hasura/graphql-engine/cli/migrate/cmd"
	"github.com/sirupsen/logrus"
)

const (
	DataAPIError  = "Data Error: "
	MigrationMode = "migration_mode"
)

type Response struct {
	Code       string `json:"code,omitempty"`
	Message    string `json:"message,omitempty"`
	Name       string `json:"name,omitempty"`
	StatusCode int    `json:"-"`
}

type Request struct {
	Name string        `json:"name"`
	Up   []interface{} `json:"up"`
	Down []interface{} `json:"down"`
}

func MigrateAPI(c *gin.Context) {
	// Get File url
	sourcePtr, ok := c.Get("filedir")
	if !ok {
		return
	}

	// Get hasuradb url
	databasePtr, ok := c.Get("dbpath")
	if !ok {
		return
	}

	// Get Logger
	loggerPtr, ok := c.Get("logger")
	if !ok {
		return
	}

	// Convert to url.URL
	databaseURL := databasePtr.(*url.URL)
	sourceURL := sourcePtr.(*url.URL)
	logger := loggerPtr.(*logrus.Logger)

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
	case "POST":
		var request Request

		// Bind Request body to Request struct
		if c.BindJSON(&request) != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: "Something went wrong"})
			return
		}

		startTime := time.Now()
		// Convert to Millisecond
		timestamp := startTime.UnixNano() / int64(time.Millisecond)

		createOptions := cmd.New(timestamp, request.Name, sourceURL.Path)
		err = createOptions.SetMetaUp(request.Up)
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "create_file_error", Message: err.Error()})
			return
		}
		err = createOptions.SetMetaDown(request.Down)
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "create_file_error", Message: err.Error()})
			return
		}

		err = createOptions.Create()
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "create_file_error", Message: err.Error()})
			return
		}

		// Rescan file system
		err = t.ReScan()
		if err != nil {
			deleteErr := createOptions.Delete()
			if deleteErr != nil {
				c.JSON(http.StatusInternalServerError, &Response{Code: "delete_file_error", Message: deleteErr.Error()})
				return
			}
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}

		if err = t.Migrate(uint64(timestamp), "up"); err != nil {
			deleteErr := createOptions.Delete()
			if deleteErr != nil {
				c.JSON(http.StatusInternalServerError, &Response{Code: "delete_file_error", Message: deleteErr.Error()})
				return
			}

			if strings.HasPrefix(err.Error(), DataAPIError) {
				c.JSON(http.StatusBadRequest, &Response{Code: "data_api_error", Message: strings.TrimPrefix(err.Error(), DataAPIError)})
				return
			}

			if err == migrate.ErrNoMigrationMode {
				c.JSON(http.StatusBadRequest, &Response{Code: "migration_mode_disabled", Message: err.Error()})
				return
			}

			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}
		c.JSON(http.StatusOK, &Response{Name: fmt.Sprintf("%d_%s", timestamp, request.Name)})
	default:
		c.JSON(http.StatusMethodNotAllowed, &gin.H{"message": "Method not allowed"})
	}
}
