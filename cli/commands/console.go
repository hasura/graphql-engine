package commands

import (
	"fmt"
	"net/http"
	"os"
	"sync"

	"github.com/fatih/color"
	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/pkg/console"
	"github.com/hasura/graphql-engine/cli/pkg/templates/oss"
	"github.com/labstack/gommon/log"
	"github.com/pkg/errors"
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
	gin.SetMode(gin.ReleaseMode)
	if o.EC.Version == nil {
		return errors.New("cannot validate version, object is nil")
	}

	migrate, err := newMigrate(o.EC, false)
	if err != nil {
		return err
	}

	apiServer := console.NewAPIServer(migrate, o.Address, o.APIPort, o.EC)

	// Setup console server
	templateProvider := oss.NewOSSProvider()
	consoleTemplateVersion := templateProvider.GetConsoleTemplateVersion(o.EC.Version)
	consoleAssetsVersion := templateProvider.GetConsoleAssetsVersion(o.EC.Version)
	o.EC.Logger.Debugf("rendering console template [%s] with assets [%s]", consoleTemplateVersion, consoleAssetsVersion)

	adminSecretHeader := getAdminSecretHeaderName(o.EC.Version)
	consoleRouter, err := console.BuildConsoleRouter(templateProvider, consoleTemplateVersion, o.StaticDir, gin.H{
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

	// get HTTP servers
	apiHTTPServer := apiServer.GetHTTPServer()
	consoleHTTPServer, err := consoleServer.GetHTTPServer()
	if err != nil {
		return errors.Wrap(err, "cannot create console server")
	}
	// Create WaitGroup for running 2 servers
	wg := &sync.WaitGroup{}
	o.WG = wg
	wg.Add(1)
	go func() {
		if err := apiHTTPServer.ListenAndServe(); err != nil {
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
		if err := consoleHTTPServer.ListenAndServe(); err != nil {
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
