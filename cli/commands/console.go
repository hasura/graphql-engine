package commands

import (
	"fmt"
	"net/http"
	"net/url"
	"sync"

	"github.com/fatih/color"
	"github.com/gin-contrib/cors"
	"github.com/gin-contrib/static"
	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate/api"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/hasura/graphql-engine/cli/version"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/skratchdot/open-golang/open"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// NewConsoleCmd returns the console command
func NewConsoleCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &consoleOptions{
		EC: ec,
	}
	consoleCmd := &cobra.Command{
		Use:   "console",
		Short: "Open console to manage database and try out APIs",
		Long:  "Run a web server to serve Hasura Console for GraphQL Engine to manage database and build queries",
		Example: `  # Start console:
  hasura console

  # Start console on a different address and ports:
  hasura console --address 0.0.0.0 --console-port 8080 --api-port 8081`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.run()
		},
	}
	f := consoleCmd.Flags()

	f.StringVar(&opts.APIPort, "api-port", "9693", "port for serving migrate api")
	f.StringVar(&opts.ConsolePort, "console-port", "9695", "port for serving console")
	f.StringVar(&opts.Address, "address", "localhost", "address to serve console and migration API from")
	f.BoolVar(&opts.DontOpenBrowser, "no-browser", false, "do not automatically open console in browser")
	f.StringVar(&opts.StaticDir, "static-dir", "", "directory where static assets mentioned in the console html template can be served from")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
	v.BindPFlag("admin_secret", f.Lookup("admin-secret"))
	v.BindPFlag("access_key", f.Lookup("access-key"))
	return consoleCmd
}

type consoleOptions struct {
	EC *cli.ExecutionContext

	APIPort     string
	ConsolePort string
	Address     string

	DontOpenBrowser bool

	WG *sync.WaitGroup

	StaticDir string
}

func (o *consoleOptions) run() error {
	log := o.EC.Logger
	// Switch to "release" mode in production.
	gin.SetMode(gin.ReleaseMode)

	// An Engine instance with the Logger and Recovery middleware already attached.
	r := gin.New()

	r.Use(allowCors())

	// My Router struct
	router := &cRouter{
		r,
	}

	if o.EC.Version == nil {
		return errors.New("cannot validate version, object is nil")
	}

	metadataPath, err := o.EC.GetMetadataFilePath("yaml")
	if err != nil {
		return err
	}

	router.setRoutes(o.EC.ServerConfig.ParsedEndpoint, o.EC.ServerConfig.AdminSecret, o.EC.MigrationDir, metadataPath, o.EC.Logger, o.EC.Version)

	consoleTemplateVersion := o.EC.Version.GetConsoleTemplateVersion()
	consoleAssetsVersion := o.EC.Version.GetConsoleAssetsVersion()

	o.EC.Logger.Debugf("rendering console template [%s] with assets [%s]", consoleTemplateVersion, consoleAssetsVersion)

	adminSecretHeader := getAdminSecretHeaderName(o.EC.Version)

	consoleRouter, err := serveConsole(consoleTemplateVersion, o.StaticDir, gin.H{
		"apiHost":         "http://" + o.Address,
		"apiPort":         o.APIPort,
		"cliVersion":      o.EC.Version.GetCLIVersion(),
		"serverVersion":   o.EC.Version.GetServerVersion(),
		"dataApiUrl":      o.EC.ServerConfig.ParsedEndpoint.String(),
		"dataApiVersion":  "",
		"hasAccessKey":    adminSecretHeader == XHasuraAccessKey,
		"adminSecret":     o.EC.ServerConfig.AdminSecret,
		"assetsVersion":   consoleAssetsVersion,
		"enableTelemetry": o.EC.GlobalConfig.EnableTelemetry,
		"cliUUID":         o.EC.GlobalConfig.UUID,
	})
	if err != nil {
		return errors.Wrap(err, "error serving console")
	}

	// Create WaitGroup for running 3 servers
	wg := &sync.WaitGroup{}
	o.WG = wg
	wg.Add(1)
	go func() {
		err = router.Run(o.Address + ":" + o.APIPort)
		if err != nil {
			o.EC.Logger.WithError(err).Errorf("error listening on port %s", o.APIPort)
		}
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		err = consoleRouter.Run(o.Address + ":" + o.ConsolePort)
		if err != nil {
			o.EC.Logger.WithError(err).Errorf("error listening on port %s", o.ConsolePort)
		}
		wg.Done()
	}()

	consoleURL := fmt.Sprintf("http://%s:%s/", o.Address, o.ConsolePort)

	if !o.DontOpenBrowser {
		o.EC.Spin(color.CyanString("Opening console using default browser..."))
		defer o.EC.Spinner.Stop()

		err = open.Run(consoleURL)
		if err != nil {
			o.EC.Logger.WithError(err).Warn("Error opening browser, try to open the url manually?")
		}
	}

	o.EC.Spinner.Stop()
	log.Infof("console running at: %s", consoleURL)

	o.EC.Telemetry.Beam()

	wg.Wait()
	return nil
}

type cRouter struct {
	*gin.Engine
}

func (router *cRouter) setRoutes(nurl *url.URL, adminSecret, migrationDir, metadataFile string, logger *logrus.Logger, v *version.Version) {
	apis := router.Group("/apis")
	{
		apis.Use(setLogger(logger))
		apis.Use(setFilePath(migrationDir))
		apis.Use(setDataPath(nurl, getAdminSecretHeaderName(v), adminSecret))
		// Migrate api endpoints and middleware
		migrateAPIs := apis.Group("/migrate")
		{
			settingsAPIs := migrateAPIs.Group("/settings")
			{
				settingsAPIs.Any("", api.SettingsAPI)
			}
			migrateAPIs.Any("", api.MigrateAPI)
		}
		// Migrate api endpoints and middleware
		metadataAPIs := apis.Group("/metadata")
		{
			metadataAPIs.Use(setMetadataFile(metadataFile))
			metadataAPIs.Any("", api.MetadataAPI)
		}
	}
}

func setDataPath(nurl *url.URL, adminSecretHeader, adminSecret string) gin.HandlerFunc {
	return func(c *gin.Context) {
		host := getDataPath(nurl, adminSecretHeader, adminSecret)

		c.Set("dbpath", host)
		c.Next()
	}
}

func setFilePath(dir string) gin.HandlerFunc {
	return func(c *gin.Context) {
		host := getFilePath(dir)
		c.Set("filedir", host)
		c.Next()
	}
}

func setMetadataFile(file string) gin.HandlerFunc {
	return func(c *gin.Context) {
		c.Set("metadataFile", file)
		c.Next()
	}
}

func setLogger(logger *logrus.Logger) gin.HandlerFunc {
	return func(c *gin.Context) {
		c.Set("logger", logger)
		c.Next()
	}
}

func allowCors() gin.HandlerFunc {
	config := cors.DefaultConfig()
	config.AddAllowHeaders("X-Hasura-User-Id")
	config.AddAllowHeaders(XHasuraAccessKey)
	config.AddAllowHeaders(XHasuraAdminSecret)
	config.AddAllowHeaders("X-Hasura-Role")
	config.AddAllowHeaders("X-Hasura-Allowed-Roles")
	config.AddAllowMethods("DELETE")
	config.AllowAllOrigins = true
	config.AllowCredentials = false
	return cors.New(config)
}

func serveConsole(assetsVersion, staticDir string, opts gin.H) (*gin.Engine, error) {
	// An Engine instance with the Logger and Recovery middleware already attached.
	r := gin.New()

	// Template console.html
	templateRender, err := util.LoadTemplates("assets/"+assetsVersion+"/", "console.html")
	if err != nil {
		return nil, errors.Wrap(err, "cannot fetch template")
	}
	r.HTMLRender = templateRender

	if staticDir != "" {
		r.Use(static.Serve("/static", static.LocalFile(staticDir, false)))
		opts["cliStaticDir"] = staticDir
	}
	r.GET("/*action", func(c *gin.Context) {
		c.HTML(http.StatusOK, "console.html", &opts)
	})

	return r, nil
}
