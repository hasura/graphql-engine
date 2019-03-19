package commands

import (
	"context"
	"fmt"
	"net/http"
	"net/url"
	"runtime/debug"
	"sync"
	"time"

	"github.com/fatih/color"
	"github.com/gin-contrib/cors"
	"github.com/gin-contrib/static"
	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate/api"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/mholt/caddy/caddyhttp/httpserver"
	caddyproxy "github.com/mholt/caddy/caddyhttp/proxy"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/skratchdot/open-golang/open"
	"github.com/spf13/cobra"
	"github.com/spf13/pflag"
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
	opts.flags = f

	f.StringVar(&opts.APIPort, "api-port", "9693", "port for serving migrate api")
	f.StringVar(&opts.ConsolePort, "console-port", "9695", "port for serving console")
	f.StringVar(&opts.Address, "address", "localhost", "address to serve console and migration API from")
	f.BoolVar(&opts.DontOpenBrowser, "no-browser", false, "do not automatically open console in browser")
	f.StringVar(&opts.StaticDir, "static-dir", "", "directory where static assets mentioned in the console html template can be served from")
	f.StringVar(&opts.apiExternalURL, "api-external-url", "http://localhost:9693", "")

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
	EC    *cli.ExecutionContext
	flags *pflag.FlagSet

	APIPort     string
	ConsolePort string
	Address     string

	DontOpenBrowser bool

	WG *sync.WaitGroup

	StaticDir      string
	enableProxy    bool
	apiExternalURL string
}

func (o *consoleOptions) run() error {
	// Switch to "release" mode in production.
	gin.SetMode(gin.ReleaseMode)

	// An Engine instance with the Logger and Recovery middleware already attached.
	r := gin.New()

	r.Use(allowCors())

	apiURL := &url.URL{
		Host:   fmt.Sprintf("%s:%s", o.Address, o.APIPort),
		Scheme: "http",
	}

	if o.flags.Changed("api-external-url") {
		nurl, err := url.Parse(o.apiExternalURL)
		if err != nil {
			return errors.Wrap(err, "cannot parse API external url")
		}
		apiURL = nurl
		o.enableProxy = true
	}

	// Console Router
	router := &cRouter{
		engine:      r,
		enableProxy: o.enableProxy,
		ec:          o.EC,
	}

	if o.EC.Version == nil {
		return errors.New("cannot validate version, object is nil")
	}

	router.setRoutes()

	consoleTemplateVersion := o.EC.Version.GetConsoleTemplateVersion()
	consoleAssetsVersion := o.EC.Version.GetConsoleAssetsVersion()

	o.EC.Logger.Debugf("rendering console template [%s] with assets [%s]", consoleTemplateVersion, consoleAssetsVersion)

	adminSecretHeader := getAdminSecretHeaderName(o.EC.Version)

	opts := gin.H{
		"apiURL":          apiURL.String(),
		"cliVersion":      o.EC.Version.GetCLIVersion(),
		"dataApiUrl":      o.EC.ServerConfig.ParsedEndpoint.String(),
		"dataApiVersion":  "",
		"hasAccessKey":    adminSecretHeader == XHasuraAccessKey,
		"adminSecret":     o.EC.ServerConfig.AdminSecret,
		"assetsVersion":   consoleAssetsVersion,
		"enableTelemetry": o.EC.GlobalConfig.EnableTelemetry,
		"cliUUID":         o.EC.GlobalConfig.UUID,
	}

	if o.enableProxy {
		opts["proxyPath"] = "/apis/proxy"
	}

	consoleRouter, err := serveConsole(consoleTemplateVersion, o.StaticDir, opts)
	if err != nil {
		return errors.Wrap(err, "error serving console")
	}

	// Create WaitGroup for running 2 servers
	wg := &sync.WaitGroup{}
	o.WG = wg
	wg.Add(1)
	go func() {
		err = router.engine.Run(o.Address + ":" + o.APIPort)
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
	o.EC.Logger.Infof("console running at: %s", consoleURL)

	o.EC.Telemetry.Beam()

	wg.Wait()
	return nil
}

type cRouter struct {
	engine *gin.Engine

	enableProxy bool
	ec          *cli.ExecutionContext
}

func (router *cRouter) setRoutes() {
	apis := router.engine.Group("/apis")
	{
		apis.Use(setLogger(router.ec.Logger))
		apis.Use(setFilePath(router.ec.MigrationDir))
		apis.Use(setDataPath(router.ec.ServerConfig.ParsedEndpoint, getAdminSecretHeaderName(router.ec.Version), router.ec.ServerConfig.AdminSecret))
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
			metadataAPIs.Use(setMetadataFile(router.ec.MetadataFile))
			metadataAPIs.Any("", api.MetadataAPI)
		}
		// Server Proxy
		if router.enableProxy {
			proxyAPIs := apis.Group("/proxy")
			{
				// Proxy Request to server
				proxyAPIs.Any("*action", func(c *gin.Context) {
					timeout := 30 * time.Second
					delay := 300 * time.Millisecond
					proxy := caddyproxy.NewSingleHostReverseProxy(router.ec.ServerConfig.ParsedEndpoint, "/apis/proxy", 0, timeout, delay)
					var err error

					// capture panic from ServeHTTP
					defer func() {
						r := recover()
						if err, ok := r.(httpserver.NonHijackerError); ok {
							router.ec.Logger.Debugf("got non-hijack error %s", err.Error())
							c.AbortWithStatus(http.StatusBadRequest)
						} else {
							router.ec.Logger.Debugf("panic recevied %s", string(debug.Stack()))
							c.AbortWithStatus(http.StatusInternalServerError)
						}
					}()

					err = proxy.ServeHTTP(c.Writer, c.Request, nil)
					if err == nil {
						return
					}

					if err == httpserver.ErrMaxBytesExceeded {
						c.AbortWithError(http.StatusRequestEntityTooLarge, err)
						return
					}

					if err == context.Canceled {
						c.AbortWithError(caddyproxy.CustomStatusContextCancelled, err)
						return
					}
				})
			}
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
	config.AllowHeaders = []string{"*"}
	config.AllowOrigins = []string{"*"}
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
