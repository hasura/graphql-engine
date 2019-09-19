package api

import (
	"net/http"
	"net/url"
	"strings"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/hasura/graphql-engine/cli/migrate/cmd"
)

type squashCreateRequest struct {
	Name    string `json:"name"`
	From    uint64 `json:"from"`
	version int64
}

func (s *squashCreateRequest) setDefaults() {
	if s.Name == "" {
		s.Name = "default_squash"
	}
	startTime := time.Now()
	s.version = startTime.UnixNano() / int64(time.Millisecond)
}

func SquashCreateAPI(c *gin.Context) {
	migratePtr, ok := c.Get("migrate")
	if !ok {
		return
	}

	sourcePtr, ok := c.Get("filedir")
	if !ok {
		return
	}

	t := migratePtr.(*migrate.Migrate)
	sourceURL := sourcePtr.(*url.URL)

	var request squashCreateRequest
	// Bind Request body to Request struct
	if c.BindJSON(&request) != nil {
		c.JSON(500, &Response{Code: "internal_error", Message: "Something went wrong"})
		return
	}
	request.setDefaults()
	err := cmd.SquashCmd(t, request.From, request.version, request.Name, sourceURL.Path)
	if err != nil {
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

	c.JSON(http.StatusOK, gin.H{"version": request.version})
}
