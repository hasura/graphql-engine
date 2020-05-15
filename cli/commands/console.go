package commands

import (
	"os"
	"sync"

	"github.com/hasura/graphql-engine/cli/util"

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
	f.StringVar(&opts.Address, "address", "localhost", "address to serve console and migration API from")
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

	APIPort     string
	ConsolePort string
	Address     string

	DontOpenBrowser bool

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
	o.EC.Logger.Debugf("rendering console template [%s] with assets [%s]", consoleTemplateVersion, consoleAssetsVersion)

	adminSecretHeader := cli.GetAdminSecretHeaderName(o.EC.Version)
	if o.EC.Config.ServerConfig.HasuraServerInternalConfig.ConsoleAssetsDir != "" {
		o.UseServerAssets = true
	}

	consoleRouter, err := console.BuildConsoleRouter(templateProvider, consoleTemplateVersion, o.StaticDir, gin.H{
		"apiHost":         "http://" + o.Address,
		"apiPort":         o.APIPort,
		"cliVersion":      o.EC.Version.GetCLIVersion(),
		"serverVersion":   o.EC.Version.GetServerVersion(),
		"dataApiUrl":      o.EC.Config.ServerConfig.ParsedEndpoint.String(),
		"dataApiVersion":  "",
		"hasAccessKey":    adminSecretHeader == cli.XHasuraAccessKey,
		"adminSecret":     o.EC.Config.ServerConfig.AdminSecret,
		"assetsVersion":   consoleAssetsVersion,
		"enableTelemetry": o.EC.GlobalConfig.EnableTelemetry,
		"cliUUID":         o.EC.GlobalConfig.UUID,
		"cdnAssets":       !o.UseServerAssets,
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
