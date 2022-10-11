package hasura

import (
	"io"
)

type PGDumpRequest struct {
	Opts        []string `json:"opts"`
	CleanOutput bool     `json:"clean_output"`
	SourceName  string   `json:"source,omitempty"`
}

type PGDump interface {
	Send(request PGDumpRequest) (responseBody io.Reader, error error)
}
