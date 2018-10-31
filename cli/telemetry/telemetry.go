package telemetry

import "sync"

// Waiter waits for telemetry ops to complete, if required
var Waiter sync.WaitGroup

type telemetryPayload struct {
	Topic string        `json:"topic"`
	Data  TelemetryData `json:"data"`
}

// TelemetryData holds all info collected and transmitted
type TelemetryData struct {
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

	// Command being executed.
	Command string `json:"command"`

	// Indicates whether the execution resulted in an error or not.
	IsError bool `json:"is_error"`

	// Any additional payload information.
	Payload map[string]interface{} `json:"payload"`
}
