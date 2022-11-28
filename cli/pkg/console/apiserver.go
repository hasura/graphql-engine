package console

import (
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"

	"github.com/gin-contrib/cors"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/migrate"
	"github.com/hasura/graphql-engine/cli/v2/migrate/api"
	_ "github.com/hasura/graphql-engine/cli/v2/migrate/database/hasuradb"
	"github.com/sirupsen/logrus"
)

type APIServer struct {
	Router  *gin.Engine
	Migrate *migrate.Migrate

	Address string
	Port    string
	EC      *cli.ExecutionContext
}

type errMessage struct {
	ErrorMessage string `json:"error"`
}

func (e errMessage) Error() string {
	b, err := json.Marshal(e)
	if err == nil {
		return string(b)
	}
	return e.ErrorMessage
}

func cliProjectUpdateCheck(ec *cli.ExecutionContext) gin.HandlerFunc {
	const updateRequiredMessage = "looks like you are trying to use hasura with multiple databases, this requires changes to your project directory. Please use `hasura scripts update-project-v3` to update your project"
	return func(c *gin.Context) {
		type response struct {
			Code       string `json:"code,omitempty"`
			Message    string `json:"message,omitempty"`
			Name       string `json:"name,omitempty"`
			StatusCode int    `json:"-"`
		}
		// if a user is on config v2 and they have multiple databases prompt them to upgrade
		if ec.Config.Version <= cli.V2 && ec.HasMetadataV3 {
			sources, err := metadatautil.GetSources(ec.APIClient.V1Metadata.ExportMetadata)
			if err != nil {
				c.AbortWithStatusJSON(http.StatusBadRequest, &response{Code: "internal_error", Message: errMessage{"cannot list sources"}.Error()})
				return
			}
			if len(sources) > 1 {
				r := response{
					Code:    "internal_error",
					Message: errMessage{updateRequiredMessage}.Error(),
				}
				c.AbortWithStatusJSON(http.StatusInternalServerError, &r)
				return
			}
		}
		c.Next()
	}
}

func NewAPIServer(address string, port string, ec *cli.ExecutionContext) (*APIServer, error) {
	var op errors.Op = "console.NewAPIServer"
	migrate, err := migrate.NewMigrate(ec, false, "", hasura.SourceKindPG)
	if err != nil {
		return nil, errors.E(op, fmt.Errorf("error creating migrate instance: %w", err))
	}
	gin.SetMode(gin.ReleaseMode)
	router := gin.New()
	// Setup API Router
	// Switch to "release" mode in production.
	gin.SetMode(gin.ReleaseMode)
	// An Engine instance with the Logger and Recovery middleware already attached.
	router.Use(allowCors())
	router.Use(cliProjectUpdateCheck(ec))

	apiServer := &APIServer{Router: router, Migrate: migrate, Address: address, Port: port, EC: ec}
	apiServer.setRoutes(ec.MigrationDir, ec.Logger)
	return apiServer, nil
}

func (r *APIServer) GetHTTPServer() *http.Server {
	// create servers
	return &http.Server{
		Addr:    fmt.Sprintf("%s:%s", r.Address, r.Port),
		Handler: r.Router,
	}
}

func (r *APIServer) setRoutes(migrationDir string, logger *logrus.Logger) {

	apis := r.Router.Group("/apis")
	{
		apis.Use(r.setLogger(logger))
		apis.Use(r.setFilePath(migrationDir))
		apis.Use(r.setMigrate(r.Migrate))
		apis.Use(r.setEC(r.EC))
		apis.Use(r.setConfigVersion(r.EC.Config.Version))
		// Migrate api endpoints and middleware
		migrateAPIs := apis.Group("/migrate")
		{
			settingsAPIs := migrateAPIs.Group("/settings")
			{
				settingsAPIs.Any("", api.SettingsAPI)
			}
			squashAPIs := migrateAPIs.Group("/squash")
			{
				squashAPIs.POST("/create", api.SquashCreateAPI)
				squashAPIs.POST("/delete", api.SquashDeleteAPI)
			}
			migrateAPIs.Any("", api.MigrateAPI)
		}
		// Migrate api endpoints and middleware
		metadataAPIs := apis.Group("/metadata")
		{
			metadataAPIs.Any("", api.MetadataAPI)
		}
	}
}

func (r *APIServer) setMigrate(t *migrate.Migrate) gin.HandlerFunc {
	return func(c *gin.Context) {
		c.Set("migrate", t)
		c.Next()
	}
}

func (r *APIServer) setEC(ec *cli.ExecutionContext) gin.HandlerFunc {
	return func(c *gin.Context) {
		c.Set("ec", ec)
		c.Next()
	}
}

func (r *APIServer) setFilePath(dir string) gin.HandlerFunc {
	return func(c *gin.Context) {
		host := migrate.GetFilePath(dir)
		c.Set("filedir", host)
		c.Next()
	}
}

func (r *APIServer) setConfigVersion(configVersion cli.ConfigVersion) gin.HandlerFunc {
	return func(c *gin.Context) {
		c.Set("version", int(configVersion))
		c.Next()
	}
}

/*
func (r *APIServer) setMetadataFile(file string) gin.HandlerFunc {
	return func(c *gin.Context) {
		c.Set("metadataFile", file)
		c.Next()
	}
}
*/

func (r *APIServer) setLogger(logger *logrus.Logger) gin.HandlerFunc {
	return func(c *gin.Context) {
		c.Set("logger", logger)
		c.Next()
	}
}

func allowCors() gin.HandlerFunc {
	var config = cors.DefaultConfig()
	config.AddAllowHeaders("X-Hasura-User-Id")
	config.AddAllowHeaders(cli.XHasuraAccessKey)
	config.AddAllowHeaders(cli.XHasuraAdminSecret)
	config.AddAllowHeaders("hasura-client-name")
	config.AddAllowHeaders("hasura-collaborator-token")
	config.AddAllowHeaders("X-Hasura-Role")
	config.AddAllowHeaders("X-Hasura-Allowed-Roles")
	config.AddAllowHeaders("Hasura-Internal-Request-Source")
	config.AddAllowMethods("DELETE")
	config.AddAllowHeaders("Hasura-Internal-Request-Source")
	config.AllowAllOrigins = true
	config.AllowCredentials = false
	return cors.New(config)
}
