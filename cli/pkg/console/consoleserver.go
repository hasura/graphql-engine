package console

import (
	"fmt"
	"net/http"
	"os"

	"github.com/gin-contrib/static"

	"github.com/fatih/color"
	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/skratchdot/open-golang/open"
)

type ConsoleServer struct {
	Logger          *logrus.Logger
	Port            string
	APIPort         string
	Address         string
	DontOpenBrowser bool
	StaticDir       string
	Browser         string

	EC               *cli.ExecutionContext
	TemplateProvider TemplateProvider
	Router           *gin.Engine
}

type NewConsoleServerOpts struct {
	Logger          *logrus.Logger
	Port            string
	APIPort         string
	Address         string
	DontOpenBrowser bool
	StaticDir       string
	Browser         string

	TemplateProvider TemplateProvider
	EC               *cli.ExecutionContext
	Router           *gin.Engine
}

func NewConsoleServer(opts *NewConsoleServerOpts) *ConsoleServer {
	return &ConsoleServer{
		Logger:          opts.Logger,
		Port:            opts.Port,
		Address:         opts.Address,
		APIPort:         opts.APIPort,
		DontOpenBrowser: opts.DontOpenBrowser,
		StaticDir:       opts.StaticDir,
		Browser:         opts.Browser,
		EC:              opts.EC,

		TemplateProvider: opts.TemplateProvider,
		Router:           opts.Router,
	}
}

func (c *ConsoleServer) GetHTTPServer() (*http.Server, error) {
	// Switch to "release" mode in production.
	gin.SetMode(gin.ReleaseMode)

	consoleTemplateVersion := c.TemplateProvider.GetTemplateVersion(c.EC.Version)
	consoleAssetsVersion := c.TemplateProvider.GetAssetsVersion(c.EC.Version)

	c.Logger.Debugf("rendering console template [%s] with assets [%s]", consoleTemplateVersion, consoleAssetsVersion)

	consoleServer := &http.Server{
		Addr:    fmt.Sprintf("%s:%s", c.Address, c.Port),
		Handler: c.Router,
	}

	return consoleServer, nil
}

func (c *ConsoleServer) Serve() {
	server, err := c.GetHTTPServer()
	if err != nil {
		c.Logger.Fatal(errors.Wrap(err, "error starting server"))
		os.Exit(1)
	}

	go func() {
		if err := server.ListenAndServe(); err != nil {
			if err == http.ErrServerClosed {
				c.EC.Logger.Infof("server closed on port %s under signal", c.Port)
			} else {
				c.EC.Logger.WithError(err).Errorf("error listening on port %s", c.Port)
			}
		}
	}()

	consoleURL := fmt.Sprintf("http://%s:%s/", c.Address, c.Port)

	if !c.DontOpenBrowser {
		if c.Browser != "" {
			c.EC.Spin(color.CyanString("Opening console on: %s", c.Browser))
			defer c.EC.Spinner.Stop()
			err := open.RunWith(consoleURL, c.Browser)
			if err != nil {
				c.EC.Logger.WithError(err).Warnf("failed opening console in '%s', try to open the url manually", c.Browser)
			}
		} else {
			c.EC.Spin(color.CyanString("Opening console using default browser..."))
			defer c.EC.Spinner.Stop()

			err := open.Run(consoleURL)
			if err != nil {
				c.EC.Logger.WithError(err).Warn("Error opening browser, try to open the url manually?")
			}
		}
	}

	c.EC.Spinner.Stop()
	c.Logger.Infof("console running at: %s", consoleURL)
}

func BuildConsoleRouter(templateProvider TemplateProvider, templateVersion, staticDir string, opts gin.H) (*gin.Engine, error) {
	// An Engine instance with the Logger and Recovery middleware already attached.
	gin.SetMode(gin.ReleaseMode)
	r := gin.New()
	if !templateProvider.DoTemplateExist(templateProvider.BasePath() + templateVersion + templateProvider.TemplateFilename()) {
		templateVersion = "latest"
	}
	// Template console.gohtml
	templateRender, err := templateProvider.LoadTemplates(templateProvider.BasePath()+templateVersion+"/", templateProvider.TemplateFilename())
	if err != nil {
		return nil, errors.Wrap(err, "cannot fetch template")
	}
	r.HTMLRender = templateRender

	if staticDir != "" {
		r.Use(static.Serve("/static", static.LocalFile(staticDir, false)))
		opts["cliStaticDir"] = staticDir
	}
	r.GET("/*action", func(c *gin.Context) {
		c.HTML(http.StatusOK, templateProvider.TemplateFilename(), &opts)
	})

	return r, nil
}
