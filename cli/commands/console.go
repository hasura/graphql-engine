package commands

import (
	"net/http"
	"os"
	"sync"

	"github.com/gin-contrib/cors"
	"github.com/gin-contrib/static"
	"github.com/hasura/graphql-engine/cli/migrate"
	"github.com/hasura/graphql-engine/cli/migrate/api"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/sirupsen/logrus"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/pkg/console"
	"github.com/pkg/errors"
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
	f.StringVar(&opts.Address, "address", "localhost", "address to serve console and migrate api from")
	f.StringVar(&opts.ServerExternalEndpoint, "server-external-endpoint", "", "endpoint using which console can access graphql engine")
	f.StringVar(&opts.CliExternalEndpoint, "cli-external-endpoint", "", "endpoint using which console can access the migrate api served by cli")
	f.BoolVar(&opts.DontOpenBrowser, "no-browser", false, "do not automatically open console in browser")
	f.StringVar(&opts.StaticDir, "static-dir", "", "directory where static assets mentioned in the console html template can be served from")
	f.StringVar(&opts.Browser, "browser", "", "open console in a specific browser")
	f.BoolVar(&opts.UseServerAssets, "use-server-assets", false, "when rendering console, use assets provided by HGE server")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	f.MarkDeprecated("access-key", "use --admin-secret instead")
	f.Bool("insecure-skip-tls-verify", false, "skip TLS verification and disable cert checking (default: false)")
	f.String("certificate-authority", "", "path to a cert file for the certificate authority")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	util.BindPFlag(v, "endpoint", f.Lookup("endpoint"))
	util.BindPFlag(v, "admin_secret", f.Lookup("admin-secret"))
	util.BindPFlag(v, "access_key", f.Lookup("access-key"))
	util.BindPFlag(v, "insecure_skip_tls_verify", f.Lookup("insecure-skip-tls-verify"))
	util.BindPFlag(v, "certificate_authority", f.Lookup("certificate-authority"))

	return consoleCmd
}

type ConsoleOptions struct {
	EC *cli.ExecutionContext

	APIPort                string
	ConsolePort            string
	Address                string
	ServerExternalEndpoint string
	CliExternalEndpoint    string
	DontOpenBrowser        bool

	WG *sync.WaitGroup

	StaticDir       string
	Browser         string
	UseServerAssets bool

	APIServerInterruptSignal     chan os.Signal
	ConsoleServerInterruptSignal chan os.Signal
}

func (o *ConsoleOptions) Run() error {
	if o.EC.Version == nil {
		return errors.New("cannot validate version, object is nil")
	}

	apiServer, err := console.NewAPIServer(o.Address, o.APIPort, o.EC)
	if err != nil {
		return err
	}

	// Setup console server
	const basePath = "/pkg/console/templates/gohtml/"
	const templateFilename = "console.gohtml"
	templateProvider := console.NewDefaultTemplateProvider(basePath, templateFilename)
	consoleTemplateVersion := templateProvider.GetTemplateVersion(o.EC.Version)
	consoleAssetsVersion := templateProvider.GetAssetsVersion(o.EC.Version)

	if o.CliExternalEndpoint == "" {
		o.CliExternalEndpoint = "http://" + o.Address + ":" + o.APIPort
	}
	if o.ServerExternalEndpoint == "" {
		o.ServerExternalEndpoint = o.EC.Config.ServerConfig.Endpoint
	}

	// FIXME: My Router struct
	r := &cRouter{
		g,
		t,
	}

	r.router.Use(verifyAdminSecret())
	r.setRoutes(o.EC.MigrationDir, o.EC.Logger)

	// consoleTemplateVersion := o.EC.Version.GetConsoleTemplateVersion()
	// consoleAssetsVersion := o.EC.Version.GetConsoleAssetsVersion()

	o.EC.Logger.Debugf("rendering console template [%s] with assets [%s]", consoleTemplateVersion, consoleAssetsVersion)

	adminSecretHeader := cli.GetAdminSecretHeaderName(o.EC.Version)
	if o.EC.Config.ServerConfig.HasuraServerInternalConfig.ConsoleAssetsDir != "" {
		o.UseServerAssets = true
	}

	consoleRouter, err := console.BuildConsoleRouter(templateProvider, consoleTemplateVersion, o.StaticDir, gin.H{
		"apiHost":              "http://" + o.Address,
		"apiPort":              o.APIPort,
		"cliVersion":           o.EC.Version.GetCLIVersion(),
		"serverVersion":        o.EC.Version.GetServerVersion(),
		"dataApiUrl":           o.EC.Config.ServerConfig.ParsedEndpoint.String(),
		"dataApiVersion":       "",
		"hasAccessKey":         adminSecretHeader == cli.XHasuraAccessKey,
		"adminSecret":          o.EC.Config.ServerConfig.AdminSecret,
		"assetsVersion":        consoleAssetsVersion,
		"enableTelemetry":      o.EC.GlobalConfig.EnableTelemetry,
		"cliUUID":              o.EC.GlobalConfig.UUID,
		"migrateSkipExecution": true,
		"cdnAssets":            !o.UseServerAssets,
		"consolePath":          "/console",
		"urlPrefix":            "/console",
	})
	if err != nil {
		return errors.Wrap(err, "error serving console")
	}
	consoleServer := console.NewConsoleServer(&console.NewConsoleServerOpts{
		Logger:           o.EC.Logger,
		APIPort:          o.APIPort,
		Address:          o.Address,
		Browser:          o.Browser,
		DontOpenBrowser:  o.DontOpenBrowser,
		EC:               o.EC,
		Port:             o.ConsolePort,
		Router:           consoleRouter,
		StaticDir:        o.StaticDir,
		TemplateProvider: templateProvider,
	})

	o.WG = new(sync.WaitGroup)
	// start console and API HTTP Servers
	serveOpts := &console.ServeOpts{
		APIServer:               apiServer,
		ConsoleServer:           consoleServer,
		EC:                      o.EC,
		DontOpenBrowser:         o.DontOpenBrowser,
		Browser:                 o.Browser,
		ConsolePort:             o.ConsolePort,
		APIPort:                 o.APIPort,
		Address:                 o.Address,
		SignalChanConsoleServer: o.ConsoleServerInterruptSignal,
		SignalChanAPIServer:     o.APIServerInterruptSignal,
		WG:                      o.WG,
	}

	return console.Serve(serveOpts)
}

func verifyAdminSecret() gin.HandlerFunc {
	return func(c *gin.Context) {
		if ec.Config.ServerConfig.AdminSecret != "" {
			if c.GetHeader(cli.XHasuraAdminSecret) != ec.Config.ServerConfig.AdminSecret {
				//reject
				c.AbortWithStatusJSON(http.StatusUnauthorized, gin.H{"message": "unauthorized"})
			}
		}
	}
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
		// FIXME
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
	config.AddAllowHeaders(cli.XHasuraAccessKey)
	config.AddAllowHeaders(cli.XHasuraAdminSecret)
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

	// FIXME: DoAssetExist
	if !util.DoAssetExist("assets/" + assetsVersion + "/console.html") {
		assetsVersion = "latest"
	}

	// Template console.html
	// FIXME: LoadTemplates
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
