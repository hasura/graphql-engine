package telemetry

import (
	"runtime"
	"sync"
	"time"

	"github.com/hasura/graphql-engine/cli"
	"github.com/hasura/graphql-engine/cli/version"
	"github.com/parnurzeal/gorequest"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

// Waiter waits for telemetry ops to complete, if required
var Waiter sync.WaitGroup

// Endpoint is where telemetry data is sent.
const Endpoint = "https://telemetry.hasura.io/v1/http"

// Topic is the name under which telemetry is sent.
var Topic = "cli_test"

func init() {
	var v = version.New()
	if v.CLISemver != nil {
		Topic = "cli"
	}
}

type requestPayload struct {
	Topic string `json:"topic"`
	Data  `json:"data"`
}

// Data holds all info collected and transmitted
type Data struct {
	// UUID used for telemetry, generated on first run.
	UUID string `json:"uuid"`

	// UUID obtained from server.
	ServerUUID string `json:"server_uuid"`

	// Unique id for the current execution.
	ExecutionID string `json:"execution_id"`

	// OS platform and architecture.
	OSPlatform string `json:"os_platform"`
	OSArch     string `json:"os_arch"`

	// Current cli version.
	Version string `json:"version"`

	// Current Server version.
	ServerVersion string `json:"server_version"`

	// Command being executed.
	Command string `json:"command"`

	// Indicates whether the execution resulted in an error or not.
	IsError bool `json:"is_error"`

	// Any additional payload information.
	Payload map[string]interface{} `json:"payload"`
}

// SendExecutionEvent sends the execution event for cmd to the telemtry server
func SendExecutionEvent(ec *cli.ExecutionContext, cmd *cobra.Command, args []string, payload map[string]interface{}) {
	if ec.GlobalConfig == nil {
		return
	}
	if ec.GlobalConfig.DisableCLITelemetry {
		ec.Logger.Debugf("telemtry is disabled, not sending data")
		return
	}
	data := makeData(ec)
	data.Command = cmd.CommandPath()
	data.Payload = payload
	beam(&data, ec.Logger)
}

// SendErrorEvent makes a telemtry call indicating the current execution
// resulted in an error.
func SendErrorEvent(ec *cli.ExecutionContext, payload map[string]interface{}) {
	if ec.GlobalConfig == nil {
		return
	}
	if ec.GlobalConfig.DisableCLITelemetry {
		ec.Logger.Debugf("telemtry is disabled, not sending data")
		return
	}
	data := makeData(ec)
	data.IsError = true
	data.Payload = payload
	beam(&data, ec.Logger)
}

func makeData(ec *cli.ExecutionContext) Data {
	return Data{
		OSPlatform:  runtime.GOOS,
		OSArch:      runtime.GOARCH,
		Version:     version.BuildVersion,
		UUID:        ec.GlobalConfig.UUID,
		ExecutionID: ec.ID,
	}
}

func beam(d *Data, log *logrus.Logger) {
	p := requestPayload{
		Topic: Topic,
		Data:  *d,
	}
	tick := time.Now()
	_, _, err := gorequest.New().
		Post(Endpoint).
		Timeout(2 * time.Second).
		Send(p).
		End()
	if err != nil {
		log.Debugf("sending telemetry payload failed: %v", err)
	}
	tock := time.Now()
	delta := tock.Sub(tick)
	log.WithField("isError", d.IsError).WithField("time", delta.String()).Debug("telemetry sent")
}
