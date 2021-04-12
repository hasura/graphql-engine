package console

import (
	"fmt"
	"net/http"

	"github.com/pkg/errors"

	"github.com/gin-contrib/cors"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/hasura/graphql-engine/cli/migrate/api"
	_ "github.com/hasura/graphql-engine/cli/migrate/database/hasuradb"
	"github.com/sirupsen/logrus"
)

type APIServer struct {
	Router  *gin.Engine
	Migrate *migrate.Migrate

	Address string
	Port    string
	EC      *cli.ExecutionContext
}

func NewAPIServer(address string, port string, ec *cli.ExecutionContext) (*APIServer, error) {
	migrate, err := migrate.NewMigrate(ec, false)
	if err != nil {
		return nil, errors.Wrap(err, "error creating migrate instance")
	}
	gin.SetMode(gin.ReleaseMode)
	router := gin.New()
	// Setup API Router
	// Switch to "release" mode in production.
	gin.SetMode(gin.ReleaseMode)
	// An Engine instance with the Logger and Recovery middleware already attached.
	router.Use(allowCors())

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

func (r *APIServer) setMetadataFile(file string) gin.HandlerFunc {
	return func(c *gin.Context) {
		c.Set("metadataFile", file)
		c.Next()
	}
}

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
	config.AddAllowMethods("DELETE")
	config.AllowAllOrigins = true
	config.AllowCredentials = false
	return cors.New(config)
}
