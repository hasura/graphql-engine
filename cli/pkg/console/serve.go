package console

import (
	stderrors "errors"
	"fmt"
	"net/http"
	"os"
	"sync"

	"github.com/fatih/color"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
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
}

// Server console and API Server.
func Serve(opts *ServeOpts) error {
	var op errors.Op = "console.Serve"
	// get HTTP servers
	apiHTTPServer := opts.APIServer.GetHTTPServer()

	consoleHTTPServer, err := opts.ConsoleServer.GetHTTPServer()
	if err != nil {
		return errors.E(op, fmt.Errorf("cannot create console server: %w", err))
	}

	go func() {
		<-opts.SignalChanAPIServer

		err := apiHTTPServer.Close()
		if err != nil {
			opts.EC.Logger.Debugf("unable to close server running on port %s", opts.APIPort)
		}
	}()

	go func() {
		<-opts.SignalChanConsoleServer

		err := consoleHTTPServer.Close()
		if err != nil {
			opts.EC.Logger.Debugf("unable to close server running on port %s", opts.ConsolePort)
		}
	}()

	// Create WaitGroup for running 2 servers
	wg := new(sync.WaitGroup)

	wg.Go(func() {
		err := apiHTTPServer.ListenAndServe()
		if err != nil {
			if stderrors.Is(err, http.ErrServerClosed) {
				opts.EC.Logger.Infof("server closed on port %s under signal", opts.APIPort)
			} else {
				opts.EC.Logger.WithError(err).Errorf("error listening on port %s", opts.APIPort)
			}
		}
	})

	wg.Go(func() {
		err := consoleHTTPServer.ListenAndServe()
		if err != nil {
			if stderrors.Is(err, http.ErrServerClosed) {
				opts.EC.Logger.Infof("server closed on port %s under signal", opts.ConsolePort)
			} else {
				opts.EC.Logger.WithError(err).Errorf("error listening on port %s", opts.ConsolePort)
			}
		}
	})

	consoleURL := fmt.Sprintf("http://%s:%s/", opts.Address, opts.ConsolePort)

	if !opts.DontOpenBrowser {
		if opts.Browser != "" {
			opts.EC.Spin(color.CyanString("Opening console on: %s", opts.Browser))
			defer opts.EC.Spinner.Stop()

			err = open.RunWith(consoleURL, opts.Browser)
			if err != nil {
				opts.EC.Logger.WithError(err).
					Warnf("failed opening console in '%s', try to open the url manually", opts.Browser)
			}
		} else {
			opts.EC.Spin(color.CyanString("Opening console using default browser..."))
			defer opts.EC.Spinner.Stop()

			err = open.Run(consoleURL)
			if err != nil {
				opts.EC.Logger.WithError(err).
					Warn("Error opening browser, try to open the url manually?")
			}
		}
	}

	opts.EC.Spinner.Stop()
	opts.EC.Logger.Infof("console running at: %s", consoleURL)

	opts.EC.Telemetry.Beam()

	wg.Wait()

	return nil
}
