package telemetry

import (
	"bytes"
	"context"
	"encoding/json"
	"io"
	"net/http"
	"runtime"
	"sync"
	"time"

	"github.com/Masterminds/semver/v3"
	"github.com/sirupsen/logrus"
)

// Waiter waits for telemetry ops to complete, if required.
var Waiter sync.WaitGroup

// Endpoint is where telemetry data is sent.
const Endpoint = "https://telemetry.hasura.io/v1/http"

// Topic is the name under which telemetry is sent.
var Topic = "cli_test"

type requestPayload struct {
	Data

	Topic string `json:"topic"`
}

// Data holds all info collected and transmitted.
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

	Error error `json:"error"`

	// Any additional payload information.
	Payload map[string]any `json:"payload"`

	// Additional objects - mandatory
	Logger *logrus.Logger `json:"-"`

	// IsBeamed indicates if this data is already beamed or not.
	IsBeamed bool `json:"-"`

	// CanBeam indicates if data can be beamed or not, e.g. disabled telemetry.
	CanBeam bool `json:"-"`
}

// BuildEvent returns a Data object which represent a telemetry event.
func BuildEvent() *Data {
	return &Data{
		OSPlatform: runtime.GOOS,
		OSArch:     runtime.GOARCH,
		CanBeam:    true,
		Error:      nil,
	}
}

// Beam the telemetry data.
func (d *Data) Beam() {
	// to be on the safe side, create a new logger if a logger
	// is not passed
	if d.Logger == nil {
		d.Logger = logrus.New()
	}

	if !d.CanBeam {
		d.Logger.Debugf("telemetry: disabled, not beaming any data")

		return
	}

	if !d.IsBeamed {
		beam(d, d.Logger)
	} else {
		d.Logger.Debugf("telemetry: data already beamed")
	}
}

func getTopic(v string) string {
	topic := "cli_test"
	if _, err := semver.NewVersion(v); err == nil {
		topic = "cli"
	}

	return topic
}

func beam(d *Data, log *logrus.Logger) {
	d.IsBeamed = true
	p := requestPayload{
		Topic: getTopic(d.Version),
		Data:  *d,
	}
	tick := time.Now()

	rawBytes, err := json.Marshal(p)
	if err != nil {
		log.Debugf("telemetry: beaming payload failed: %v", err)

		return
	}

	ctx, cancel := context.WithTimeout(context.TODO(), time.Minute)
	defer cancel()

	req, err := http.NewRequestWithContext(
		ctx,
		http.MethodPost,
		Endpoint,
		bytes.NewBuffer(rawBytes),
	)
	if err != nil {
		log.Debugf("telemetry: beaming payload failed: %v", err)

		return
	}

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		log.Debugf("telemetry: beaming payload failed: %v", err)

		return
	}

	if resp.Body != nil {
		_, _ = io.Copy(io.Discard, resp.Body)
		_ = resp.Body.Close()
	}

	tock := time.Now()
	delta := tock.Sub(tick)
	log.WithField("isError", d.IsError).WithField("time", delta.String()).Debug("telemetry: beamed")
}
