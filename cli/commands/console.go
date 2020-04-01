package commands

import (
	"fmt"
	"net/http"
	"os"
	"sync"

	"github.com/fatih/color"
	"github.com/gin-contrib/cors"
	"github.com/gin-contrib/static"
	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/hasura/graphql-engine/cli/migrate/api"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/skratchdot/open-golang/open"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// NewConsoleCmd returns the console command
func NewConsoleCmd(ec *cli.ExecutionContext) *cobra.Command {
	v := viper.New()
	opts := &ConsoleOptions{
		EC: ec,
	}
	consoleCmd := &cobra.Command{
		Use:   "console",
		Short: "Open console to manage database and try out APIs",
		Long:  "Run a web server to serve Hasura Console for GraphQL Engine to manage database and build queries",
		Example: `  # Start console:
  hasura console

  # Start console on a different address and ports:
  hasura console --address 0.0.0.0 --console-port 8080 --api-port 8081

  # Start console without opening the browser automatically
  hasura console --no-browser

  # Use with admin secret:
  hasura console --admin-secret "<admin-secret>"

  # Connect to an instance specified by the flag, overrides the one mentioned in config.yaml:
  hasura console --endpoint "<endpoint>"`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return err
			}
			return ec.Validate()
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			return opts.Run()
		},
	}
	f := consoleCmd.Flags()

	f.StringVar(&opts.APIPort, "api-port", "9693", "port for serving migrate api")
	f.StringVar(&opts.ConsolePort, "console-port", "9695", "port for serving console")
	f.StringVar(&opts.Address, "address", "localhost", "address to serve console and migration API from")
	f.BoolVar(&opts.DontOpenBrowser, "no-browser", false, "do not automatically open console in browser")
	f.StringVar(&opts.StaticDir, "static-dir", "", "directory where static assets mentioned in the console html template can be served from")
	f.StringVar(&opts.Browser, "browser", "", "open console in a specific browser")

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

type ConsoleOptions struct {
	EC *cli.ExecutionContext

	APIPort     string
	ConsolePort string
	Address     string

	DontOpenBrowser bool

	WG *sync.WaitGroup

	StaticDir string
	Browser   string

	APIServerInterruptSignal     chan os.Signal
	ConsoleServerInterruptSignal chan os.Signal
}

func (o *ConsoleOptions) Run() error {
	log := o.EC.Logger
	// Switch to "release" mode in production.
	gin.SetMode(gin.ReleaseMode)

	// An Engine instance with the Logger and Recovery middleware already attached.
	g := gin.New()

	g.Use(allowCors())

	if o.EC.Version == nil {
		return errors.New("cannot validate version, object is nil")
	}

	t, err := newMigrate(o.EC, false)
	if err != nil {
		return err
	}

	// My Router struct
	r := &cRouter{
		g,
		t,
	}

	r.setRoutes(o.EC.MigrationDir, o.EC.Logger)

	consoleTemplateVersion := o.EC.Version.GetConsoleTemplateVersion()
	consoleAssetsVersion := o.EC.Version.GetConsoleAssetsVersion()

	o.EC.Logger.Debugf("rendering console template [%s] with assets [%s]", consoleTemplateVersion, consoleAssetsVersion)

	adminSecretHeader := getAdminSecretHeaderName(o.EC.Version)

	consoleRouter, err := serveConsole(consoleTemplateVersion, o.StaticDir, gin.H{
		"apiHost":         "http://" + o.Address,
		"apiPort":         o.APIPort,
		"cliVersion":      o.EC.Version.GetCLIVersion(),
		"serverVersion":   o.EC.Version.GetServerVersion(),
		"dataApiUrl":      o.EC.Config.ServerConfig.ParsedEndpoint.String(),
		"dataApiVersion":  "",
		"hasAccessKey":    adminSecretHeader == XHasuraAccessKey,
		"adminSecret":     o.EC.Config.ServerConfig.AdminSecret,
		"assetsVersion":   consoleAssetsVersion,
		"enableTelemetry": o.EC.GlobalConfig.EnableTelemetry,
		"cliUUID":         o.EC.GlobalConfig.UUID,
	})
	if err != nil {
		return errors.Wrap(err, "error serving console")
	}

	// create servers
	apiServer := &http.Server{
		Addr:    fmt.Sprintf("%s:%s", o.Address, o.APIPort),
		Handler: r.router,
	}
	consoleServer := &http.Server{
		Addr:    fmt.Sprintf("%s:%s", o.Address, o.ConsolePort),
		Handler: consoleRouter,
	}

	go func() {
		<-o.APIServerInterruptSignal
		if err := apiServer.Close(); err != nil {
			o.EC.Logger.Debugf("unable to close server running on port %s", o.APIPort)
		}
	}()

	go func() {
		<-o.ConsoleServerInterruptSignal
		if err := consoleServer.Close(); err != nil {
			o.EC.Logger.Debugf("unable to close server running on port %s", o.ConsolePort)
		}
	}()

	// Create WaitGroup for running 2 servers
	wg := &sync.WaitGroup{}
	o.WG = wg
	wg.Add(1)
	go func() {
		if err := apiServer.ListenAndServe(); err != nil {
			if err == http.ErrServerClosed {
				o.EC.Logger.Infof("server closed on port %s under signal", o.APIPort)
			} else {
				o.EC.Logger.WithError(err).Errorf("error listening on port %s", o.APIPort)
			}
		}
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		if err := consoleServer.ListenAndServe(); err != nil {
			if err == http.ErrServerClosed {
				o.EC.Logger.Infof("server closed on port %s under signal", o.ConsolePort)
			} else {
				o.EC.Logger.WithError(err).Errorf("error listening on port %s", o.ConsolePort)
			}
		}
		wg.Done()
	}()

	consoleURL := fmt.Sprintf("http://%s:%s/", o.Address, o.ConsolePort)

	if !o.DontOpenBrowser {
		if o.Browser != "" {
			o.EC.Spin(color.CyanString("Opening console on: %s", o.Browser))
			defer o.EC.Spinner.Stop()
			err = open.RunWith(consoleURL, o.Browser)
			if err != nil {
				o.EC.Logger.WithError(err).Warnf("failed opening console in '%s', try to open the url manually", o.Browser)
			}
		} else {
			o.EC.Spin(color.CyanString("Opening console using default browser..."))
			defer o.EC.Spinner.Stop()

			err = open.Run(consoleURL)
			if err != nil {
				o.EC.Logger.WithError(err).Warn("Error opening browser, try to open the url manually?")
			}
		}
	}

	o.EC.Spinner.Stop()
	log.Infof("console running at: %s", consoleURL)

	o.EC.Telemetry.Beam()

	wg.Wait()
	return nil
}

type cRouter struct {
	router  *gin.Engine
	migrate *migrate.Migrate
}

func (r *cRouter) setRoutes(migrationDir string, logger *logrus.Logger) {
	apis := r.router.Group("/apis")
	{
		apis.Use(setLogger(logger))
		apis.Use(setFilePath(migrationDir))
		apis.Use(setMigrate(r.migrate))
		apis.Use(setConfigVersion())
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

func setMigrate(t *migrate.Migrate) gin.HandlerFunc {
	return func(c *gin.Context) {
		c.Set("migrate", t)
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

func setConfigVersion() gin.HandlerFunc {
	return func(c *gin.Context) {
		c.Set("version", int(ec.Config.Version))
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

	if !util.DoAssetExist("assets/" + assetsVersion + "/console.html") {
		assetsVersion = "latest"
	}

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
