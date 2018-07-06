package commands

import (
	"fmt"
	"net/http"
	"net/url"
	"path/filepath"
	"runtime"
	"sync"

	"github.com/fatih/color"
	"github.com/gin-contrib/cors"
	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/migrate/api"
	"github.com/hasura/graphql-engine/cli/util"
	"github.com/pkg/errors"
	"github.com/skratchdot/open-golang/open"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

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
	f.StringVar(&opts.Address, "address", "localhost", "address to use")
	f.BoolVar(&opts.DontOpenBrowser, "no-browser", false, "do not automatically open console in browser")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")

	// need to create a new viper because https://github.com/spf13/viper/issues/233
	v.BindPFlag("endpoint", f.Lookup("endpoint"))
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
}

func (o *consoleOptions) run() error {
	log := o.EC.Logger
	// Switch to "release" mode in production.
	gin.SetMode(gin.ReleaseMode)

	// An Engine instance with the Logger and Recovery middleware already attached.
	r := gin.New()

	r.Use(allowCors())

	// My Router struct
	router := &consoleRouter{
		r,
	}

	u, err := url.Parse(o.EC.Config.Endpoint)
	if err != nil {
		return errors.Wrap(err, "cannot parse endpoint as url")
	}

	router.setRoutes(u.Host, o.EC.Config.AccessKey, o.EC.MigrationDir)

	if o.EC.Version == nil {
		return errors.New("cannot validate version, object is nil")
	}
	consoleTemplateVersion := o.EC.Version.GetConsoleTemplateVersion()
	consoleAssetsVersion := o.EC.Version.GetConsoleAssetsVersion()

	o.EC.Logger.Debugf("rendering console template [%s] with assets [%s]", consoleTemplateVersion, consoleAssetsVersion)

	consoleRouter, err := serveConsole(consoleTemplateVersion, gin.H{
		"apiHost":        "http://" + o.Address,
		"apiPort":        o.APIPort,
		"cliVersion":     o.EC.Version.GetCLIVersion(),
		"dataApiUrl":     o.EC.Config.Endpoint,
		"dataApiVersion": "",
		"accessKey":      o.EC.Config.AccessKey,
		"assetsVersion":  consoleAssetsVersion,
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

	consoleURL := fmt.Sprintf("http://%s:%s", o.Address, o.ConsolePort)

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

	wg.Wait()
	return nil
}

type consoleRouter struct {
	*gin.Engine
}

func (router *consoleRouter) setRoutes(host, accessKey, migrationDir string) {
	apis := router.Group("/apis")
	{
		// Migrate api endpoints and middleware
		migrateAPIs := apis.Group("/migrate")
		{
			migrateAPIs.Use(setFilePath(migrationDir))
			migrateAPIs.Use(setDataPath(host, accessKey))
			settingsAPIs := migrateAPIs.Group("/settings")
			{
				settingsAPIs.Any("", api.SettingsAPI)
			}
			migrateAPIs.Any("", api.MigrateAPI)
		}
		// Migrate api endpoints and middleware
		metadataAPIs := apis.Group("/metadata")
		{
			metadataAPIs.Use(setFilePath(migrationDir))
			metadataAPIs.Use(setDataPath(host, accessKey))
			metadataAPIs.Any("", api.MetadataAPI)
		}
	}
}

func setDataPath(hostName, accessKey string) gin.HandlerFunc {
	return func(c *gin.Context) {
		host := url.URL{
			Scheme: "hasuradb",
			User:   url.UserPassword("admin", accessKey),
			Host:   hostName,
		}
		c.Set("dbpath", host)
		c.Next()
	}
}

func setFilePath(dir string) gin.HandlerFunc {
	return func(c *gin.Context) {
		if runtime.GOOS == "windows" {
			c.Set("filedir", "file:///"+filepath.Clean(dir))
		} else {
			c.Set("filedir", "file://"+filepath.Clean(dir))
		}
		c.Next()
	}
}

func allowCors() gin.HandlerFunc {
	config := cors.DefaultConfig()
	config.AddAllowHeaders("X-Hasura-User-Id")
	config.AddAllowHeaders("X-Hasura-Access-Key")
	config.AddAllowHeaders("X-Hasura-Role")
	config.AddAllowHeaders("X-Hasura-Allowed-Roles")
	config.AddAllowMethods("DELETE")
	config.AllowAllOrigins = true
	config.AllowCredentials = false
	return cors.New(config)
}

func serveConsole(assetsVersion string, opts gin.H) (*gin.Engine, error) {
	// An Engine instance with the Logger and Recovery middleware already attached.
	r := gin.New()

	// Template console.html
	templateRender, err := util.LoadTemplates("assets/"+assetsVersion+"/", "console.html")
	if err != nil {
		return nil, errors.Wrap(err, "cannot fetch template")
	}
	r.HTMLRender = templateRender

	r.Any("/*action", func(c *gin.Context) {
		c.HTML(http.StatusOK, "console.html", &opts)
	})

	return r, nil
}
