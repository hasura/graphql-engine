package telemetry

import (
	"sync"

	"github.com/spf13/cobra"
)

// Waiter waits for telemetry ops to complete, if required
var Waiter sync.WaitGroup

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

	// Command being executed.
	Command string `json:"command"`

	// Indicates whether the execution resulted in an error or not.
	IsError bool `json:"is_error"`

	// Any additional payload information.
	Payload map[string]interface{} `json:"payload"`
}

// SendExecutionEvent sends the execution event for cmd to the telemtry server
func SendExecutionEvent(cmd *cobra.Command, args []string) {

}
