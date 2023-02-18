package commands

import (
	"fmt"
	"net/url"
	"os"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/scripts"
	"github.com/hasura/graphql-engine/cli/v2/util"

	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/pkg/console"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

// NewConsoleCmd returns the console command
func NewConsoleCmd(ec *cli.ExecutionContext) *cobra.Command {
	var apiHost string
	v := viper.New()
	opts := &ConsoleOptions{
		EC: ec,
	}
	consoleCmd := &cobra.Command{
		Use:   "console",
		Short: "Open the Console to manage the database and try out APIs",
		Long:  "This command will start a web server to serve the Hasura Console for the GraphQL Engine. You can use this to manage the database, build queries, and try out APIs. The Console is served at http://localhost:9695 by default. You can change the port using the --console-port flag and further configure the command by including other available flags.",
		Example: `  # Start console:
  hasura console

  # Start console on a different address and ports:
  hasura console --address 0.0.0.0 --console-port 8080 --api-port 8081

  # Start console without opening the browser automatically
  hasura console --no-browser

  # Use with admin secret:
  hasura console --admin-secret "<admin-secret>"

  # Connect to an instance specified by the flag, overrides the one mentioned in config.yaml:
  hasura console --endpoint "<endpoint>"
  
  # Connect to HGE instance running in a container when running CLI inside another container:
  hasura console --endpoint <container network endpoint, like: http://host.docker.internal:8080> --no-browser --address 0.0.0.0 --console-hge-endpoint http://0.0.0.0:8080`,
		SilenceUsage: true,
		PreRunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "PreRunE")
			ec.Viper = v
			err := ec.Prepare()
			if err != nil {
				return errors.E(op, err)
			}
			if err := ec.Validate(); err != nil {
				return errors.E(op, err)
			}
			if err := scripts.CheckIfUpdateToConfigV3IsRequired(ec); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
		RunE: func(cmd *cobra.Command, args []string) error {
			op := genOpName(cmd, "RunE")
			if cmd.Flags().Changed("api-host") {
				var err error
				opts.APIHost, err = url.ParseRequestURI(apiHost)
				if err != nil {
					return errors.E(op, fmt.Errorf("expected a valid url for --api-host, parsing error: %w", err))
				}
			} else {
				opts.APIHost = &url.URL{
					Scheme: "http",
					Host:   opts.Address,
				}
			}
			if err := opts.Run(); err != nil {
				return errors.E(op, err)
			}
			return nil
		},
	}
	f := consoleCmd.Flags()

	f.StringVar(&opts.APIPort, "api-port", "9693", "port for serving migrate api")
	f.StringVar(&apiHost, "api-host", "http://localhost", "(PREVIEW: usage may change in future) host serving migrate api")
	f.StringVar(&opts.ConsolePort, "console-port", "9695", "port for serving console")
	f.StringVar(&opts.Address, "address", "localhost", "address to serve console and migration API from")
	f.BoolVar(&opts.DontOpenBrowser, "no-browser", false, "do not automatically open console in browser")
	f.StringVar(&opts.StaticDir, "static-dir", "", "directory where static assets mentioned in the console html template can be served from")
	f.StringVar(&opts.Browser, "browser", "", "open console in a specific browser")
	f.BoolVar(&opts.UseServerAssets, "use-server-assets", false, "when rendering console, use assets provided by HGE server")
	f.StringVar(&opts.DataApiUrl, "console-hge-endpoint", "", "endpoint on which the CLI Console should reach the HGE Server")

	f.String("endpoint", "", "http(s) endpoint for Hasura GraphQL Engine")
	f.String("admin-secret", "", "admin secret for Hasura GraphQL Engine")
	f.String("access-key", "", "access key for Hasura GraphQL Engine")
	if err := f.MarkDeprecated("access-key", "use --admin-secret instead"); err != nil {
		ec.Logger.WithError(err).Errorf("error while using a dependency library")
	}
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
	APIHost     *url.URL
	ConsolePort string
	Address     string

	DontOpenBrowser bool

	StaticDir       string
	Browser         string
	UseServerAssets bool
	DataApiUrl      string

	APIServerInterruptSignal     chan os.Signal
	ConsoleServerInterruptSignal chan os.Signal
}

func (o *ConsoleOptions) Run() error {
	var op errors.Op = "commands.ConsoleOptions.Run"
	if o.EC.Version == nil {
		return errors.E(op, "cannot validate version, object is nil")
	}

	apiServer, err := console.NewAPIServer(o.APIHost.Host, o.APIPort, o.EC)
	if err != nil {
		return errors.E(op, err)
	}

	var templateProvider console.TemplateProvider
	adminSecretHeader := cli.GetAdminSecretHeaderName(o.EC.Version)
	if o.EC.Config.ServerConfig.HasuraServerInternalConfig.ConsoleAssetsDir != "" {
		o.UseServerAssets = true
	}

	dataApiUrl := o.EC.Config.ServerConfig.ParsedEndpoint.String()
	if o.DataApiUrl != "" {
		// change to dataApiUrl value user entered in flag --console-hge-endpoint
		dataApiUrl = o.DataApiUrl
	}
	
	templateVars := gin.H{
		"apiHost":              o.APIHost.String(),
		"apiPort":              o.APIPort,
		"cliVersion":           o.EC.Version.GetCLIVersion(),
		"serverVersion":        o.EC.Version.GetServerVersion(),
		"dataApiUrl":           dataApiUrl,
		"dataApiVersion":       "",
		"hasAccessKey":         adminSecretHeader == cli.XHasuraAccessKey,
		"adminSecret":          o.EC.Config.ServerConfig.AdminSecret,
		"enableTelemetry":      o.EC.GlobalConfig.EnableTelemetry,
		"cliUUID":              o.EC.GlobalConfig.UUID,
		"migrateSkipExecution": true,
		"cdnAssets":            !o.UseServerAssets,
		"consolePath":          "/console",
		"urlPrefix":            "/console",
	}
	const basePath = "templates/gohtml/"
	const templateFilename = "console.gohtml"

	versionInfo, err := o.EC.APIClient.V1Version.GetVersion()
	if err != nil {
		return errors.E(op, err)
	}
	templateProvider = console.NewDefaultTemplateProvider(basePath, templateFilename, console.ConsoleFS)
	// we use the default template provider by default but
	// if we are able to find out the server type from the version API
	// introduced in: https://github.com/hasura/graphql-engine-mono/pull/7141
	// we will use that information to choose the correct server type and set the
	// template provider accordingly.
	if versionInfo.ServerType != nil {
		switch *versionInfo.ServerType {
		case "ee":
			templateVars["consoleType"] = "pro-lite"
			templateProvider = console.NewEETemplateProvider(basePath, templateFilename, console.ConsoleFS)
		case "ee-classic":
			templateVars["consoleType"] = "pro"
			templateProvider = console.NewEETemplateProvider(basePath, templateFilename, console.ConsoleFS)
		case "cloud":
			templateVars["consoleType"] = "cloud"
			templateProvider = console.NewCloudTemplateProvider(basePath, templateFilename, console.ConsoleFS)
		}
	}
	consoleTemplateVersion := templateProvider.GetTemplateVersion(o.EC.Version)
	consoleAssetsVersion := templateProvider.GetAssetsVersion(o.EC.Version)
	templateVars["assetsVersion"] = consoleAssetsVersion
	templateVars["assetsPath"] = templateProvider.GetAssetsCDN()

	// Setup console server
	o.EC.Logger.Debugf("rendering console template [%s] with assets [%s]", consoleTemplateVersion, consoleAssetsVersion)

	consoleRouter, err := console.BuildConsoleRouter(templateProvider, consoleTemplateVersion, o.StaticDir, templateVars)
	if err != nil {
		return errors.E(op, fmt.Errorf("error serving console: %w", err))
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
	}

	if err := console.Serve(serveOpts); err != nil {
		return errors.E(op, err)
	}
	return nil
}
