package api

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"path/filepath"
	"strings"
	"time"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/migrate"
	"github.com/hasura/graphql-engine/cli/v2/migrate/cmd"
	"github.com/hasura/graphql-engine/cli/v2/migrate/database/hasuradb"
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
	Name          string        `json:"name"`
	Up            []requestType `json:"up"`
	Down          []requestType `json:"down"`
	SkipExecution bool          `json:"skip_execution"`
	SourceName    string        `json:"datasource,omitempty"`
}

type requestType struct {
	Version  int         `json:"version,omitempty"`
	Type     string      `json:"type"`
	Database string      `json:"datasource,omitempty"`
	Args     interface{} `json:"args"`
}

func MigrateAPI(c *gin.Context) {
	ecPtr, ok := c.Get("ec")
	if !ok {
		return
	}
	// Get File url
	//sourcePtr, ok := c.Get("filedir")
	if !ok {
		return
	}

	// Get Logger
	loggerPtr, ok := c.Get("logger")
	if !ok {
		return
	}

	// Get version
	version := c.GetInt("version")

	// Convert to url.URL
	ec, ok := ecPtr.(*cli.ExecutionContext)
	if !ok {
		c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: "cannot get execution context"})
		return
	}
	//sourceURL := sourcePtr.(*url.URL)
	logger := loggerPtr.(*logrus.Logger)

	mdHandler := metadataobject.NewHandlerFromEC(ec)
	// Switch on request method
	switch c.Request.Method {
	case "GET":
		sourceName := c.Query("datasource")
		if ec.Config.Version >= cli.V3 && sourceName == "" {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: "datasource query parameter is required"})
			return
		}
		sourceKind := hasura.SourceKindPG
		if ec.Config.Version >= cli.V3 {
			kind, err := metadatautil.GetSourceKind(ec.APIClient.V1Metadata.ExportMetadata, sourceName)
			if err != nil {
				c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
				return
			}
			sourceKind = *kind
		}

		t, err := migrate.NewMigrate(ec, false, sourceName, sourceKind)
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}
		// Rescan file system
		err = t.ReScan()
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}
		status, err := t.GetStatus()
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: "Something went wrong"})
			return
		}
		c.JSON(http.StatusOK, status)
	case "POST":
		var request Request
		var err error

		// Bind Request body to Request struct
		if err = c.BindJSON(&request); err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "request_parse_error", Message: err.Error()})
			return
		}

		if ec.Config.Version >= cli.V3 && request.SourceName == "" {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: "datasource key not found in body"})
			return
		}

		startTime := time.Now()
		timestamp := startTime.UnixNano() / int64(time.Millisecond)
		sourceName := request.SourceName
		if ec.Config.Version < cli.V3 {
			sourceName = ""
		}
		sourceKind := hasura.SourceKindPG
		if ec.Config.Version >= cli.V3 {
			kind, err := metadatautil.GetSourceKind(ec.APIClient.V1Metadata.ExportMetadata, sourceName)
			if err != nil {
				c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
				return
			}
			sourceKind = *kind
		}
		t, err := migrate.NewMigrate(ec, false, sourceName, sourceKind)
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}
		createOptions := cmd.New(timestamp, request.Name, filepath.Join(ec.MigrationDir, sourceName))
		if version != int(cli.V1) && migrate.IsMigrationsSupported(sourceKind) {
			sqlUp := &bytes.Buffer{}
			sqlDown := &bytes.Buffer{}
			for _, arg := range request.Up {
				if strings.Contains(arg.Type, "run_sql") {
					argByt, err := json.Marshal(arg.Args)
					if err != nil {
						c.JSON(http.StatusInternalServerError, &Response{Code: "request_parse_error", Message: err.Error()})
						return
					}
					var to hasuradb.RunSQLInput
					err = json.Unmarshal(argByt, &to)
					if err != nil {
						c.JSON(http.StatusInternalServerError, &Response{Code: "request_parse_error", Message: err.Error()})
						return
					}
					sqlUp.WriteString(to.SQL)
					sqlUp.WriteString("\n")
				}
			}

			for _, arg := range request.Down {
				if strings.Contains(arg.Type, "run_sql") {
					argByt, err := json.Marshal(arg.Args)
					if err != nil {
						c.JSON(http.StatusInternalServerError, &Response{Code: "request_parse_error", Message: err.Error()})
						return
					}
					var to hasuradb.RunSQLInput
					err = json.Unmarshal(argByt, &to)
					if err != nil {
						c.JSON(http.StatusInternalServerError, &Response{Code: "request_parse_error", Message: err.Error()})
						return
					}
					sqlDown.WriteString(to.SQL)
					sqlDown.WriteString("\n")
				}
			}

			if sqlUp.String() != "" {
				err = createOptions.SetSQLUp(sqlUp.String())
				if err != nil {
					c.JSON(http.StatusInternalServerError, &Response{Code: "create_file_error", Message: err.Error()})
					return
				}
			}
			if sqlDown.String() != "" {
				err = createOptions.SetSQLDown(sqlDown.String())
				if err != nil {
					c.JSON(http.StatusInternalServerError, &Response{Code: "create_file_error", Message: err.Error()})
					return
				}
			}

			if sqlUp.String() != "" || sqlDown.String() != "" {
				err = createOptions.Create()
				if err != nil {
					c.JSON(http.StatusInternalServerError, &Response{Code: "create_file_error", Message: err.Error()})
					return
				}
			} else {
				timestamp = 0
			}
		} else {
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
		}

		defer func() {
			if err != nil && timestamp != 0 {
				err := createOptions.Delete()
				if err != nil {
					logger.Debug(err)
				}
			}
		}()

		// Rescan file system
		err = t.ReScan()
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}

		upByt, err := json.Marshal(request.Up)
		if err != nil {
			c.JSON(http.StatusInternalServerError, &Response{Code: "internal_error", Message: err.Error()})
			return
		}

		if err = t.QueryWithVersion(uint64(timestamp), ioutil.NopCloser(bytes.NewReader(upByt)), request.SkipExecution); err != nil {
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
		defer func() {
			var files map[string][]byte
			files, err = mdHandler.ExportMetadata()
			if err != nil {
				logger.Debug(err)
				return
			}
			err = mdHandler.WriteMetadata(files)
			if err != nil {
				logger.Debug(err)
				return
			}
		}()
		c.JSON(http.StatusOK, &Response{Name: fmt.Sprintf("%d_%s", timestamp, request.Name)})
	default:
		c.JSON(http.StatusMethodNotAllowed, &gin.H{"message": "Method not allowed"})
	}
}
