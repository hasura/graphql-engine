package console

import (
	"fmt"
	"net/http"
	"os"
	"sync"

	"github.com/hasura/graphql-engine/cli"

	"github.com/fatih/color"
	"github.com/pkg/errors"
	"github.com/skratchdot/open-golang/open"
)

type ServeOpts struct {
	APIServer       *APIServer
	ConsoleServer   *ConsoleServer
	EC              *cli.ExecutionContext
	DontOpenBrowser bool
	Browser         string
	ConsolePort     string
	APIPort         string
	Address         string

	SignalChanAPIServer     chan os.Signal
	SignalChanConsoleServer chan os.Signal
	WG                      *sync.WaitGroup
}

// Server console and API Server
func Serve(opts *ServeOpts) error {
	// get HTTP servers
	apiHTTPServer := opts.APIServer.GetHTTPServer()
	consoleHTTPServer, err := opts.ConsoleServer.GetHTTPServer()
	if err != nil {
		return errors.Wrap(err, "cannot create console server")
	}

	go func() {
		<-opts.SignalChanAPIServer
		if err := apiHTTPServer.Close(); err != nil {
			opts.EC.Logger.Debugf("unable to close server running on port %s", opts.APIPort)
		}
	}()

	go func() {
		<-opts.SignalChanConsoleServer
		if err := consoleHTTPServer.Close(); err != nil {
			opts.EC.Logger.Debugf("unable to close server running on port %s", opts.ConsolePort)
		}
	}()

	// Create WaitGroup for running 2 servers
	wg := opts.WG
	wg.Add(1)
	go func() {
		if err := apiHTTPServer.ListenAndServe(); err != nil {
			if err == http.ErrServerClosed {
				opts.EC.Logger.Infof("server closed on port %s under signal", opts.APIPort)
			} else {
				opts.EC.Logger.WithError(err).Errorf("error listening on port %s", opts.APIPort)
			}
		}
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		if err := consoleHTTPServer.ListenAndServe(); err != nil {
			if err == http.ErrServerClosed {
				opts.EC.Logger.Infof("server closed on port %s under signal", opts.ConsolePort)
			} else {
				opts.EC.Logger.WithError(err).Errorf("error listening on port %s", opts.ConsolePort)
			}
		}
		wg.Done()
	}()

	consoleURL := fmt.Sprintf("http://%s:%s/", opts.Address, opts.ConsolePort)

	if !opts.DontOpenBrowser {
		if opts.Browser != "" {
			opts.EC.Spin(color.CyanString("Opening console on: %s", opts.Browser))
			defer opts.EC.Spinner.Stop()
			err = open.RunWith(consoleURL, opts.Browser)
			if err != nil {
				opts.EC.Logger.WithError(err).Warnf("failed opening console in '%s', try to open the url manually", opts.Browser)
			}
		} else {
			opts.EC.Spin(color.CyanString("Opening console using default browser..."))
			defer opts.EC.Spinner.Stop()

			err = open.Run(consoleURL)
			if err != nil {
				opts.EC.Logger.WithError(err).Warn("Error opening browser, try to open the url manually?")
			}
		}
	}

	opts.EC.Spinner.Stop()
	opts.EC.Logger.Infof("console running at: %s", consoleURL)

	opts.EC.Telemetry.Beam()

	wg.Wait()
	return nil
}
