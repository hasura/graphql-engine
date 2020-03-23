package v1

import (
	"encoding/json"
	"fmt"
	"strings"
)

type Error struct {
	Err    error
	ErrAPI APIError
}

// ErrAPIJSON returns Error from API In JSON format
func (e *Error) ErrAPIJSON() string {
	return e.ErrAPI.JSON()
}

func (e *Error) ErrAPIHumanReadable() string {
	return e.ErrAPI.Error()
}

func (e *Error) Error() string {
	if e.Err != nil {
		return e.Err.Error()
	}
	return e.ErrAPI.Error()
}

func E(args ...interface{}) *Error {
	e := new(Error)
	for _, a := range args {
		switch a := a.(type) {
		case error:
			e.Err = a
		case APIError:
			e.ErrAPI = a
		default:
			e.Err = fmt.Errorf("%v", a)
		}
	}
	return e
}

// APIError maps to JSON error messages returned by Hasura server
type APIError struct {
	// MigrationFile is used internally for hasuractl
	migrationFile  string
	migrationQuery string
	Path           string            `json:"path"`
	ErrorMessage   string            `json:"error"`
	Internal       *SQLInternalError `json:"internal,omitempty"`
	Message        string            `json:"message,omitempty"`
	Code           string            `json:"code"`
	Err            error
}

type SQLInternalError struct {
	Arguments []string      `json:"arguments"`
	Error     PostgresError `json:"error"`
	Prepared  bool          `json:"prepared"`
	Statement string        `json:"statement"`
}

type PostgresError struct {
	StatusCode  string `json:"status_code"`
	ExecStatus  string `json:"exec_status"`
	Message     string `json:"message"`
	Description string `json:"description"`
	Hint        string `json:"hint"`
}

// String will transform error returned from Hasura API into
// a string of human readable format
func (h *APIError) Error() string {
	var errorStrings []string
	errorStrings = append(errorStrings, fmt.Sprintf("[%s] %s (%s)", h.Code, h.ErrorMessage, h.Path))
	if h.migrationFile != "" {
		errorStrings = append(errorStrings, fmt.Sprintf("File: '%s'", h.migrationFile))
	}
	if h.migrationQuery != "" {
		errorStrings = append(errorStrings, h.migrationQuery)
	}
	if h.Internal != nil {
		// postgres error
		errorStrings = append(errorStrings, fmt.Sprintf("[%s] %s: %s", h.Internal.Error.StatusCode, h.Internal.Error.ExecStatus, h.Internal.Error.Message))
		if len(h.Internal.Error.Description) > 0 {
			errorStrings = append(errorStrings, fmt.Sprintf("Description: %s", h.Internal.Error.Description))
		}
		if len(h.Internal.Error.Hint) > 0 {
			errorStrings = append(errorStrings, fmt.Sprintf("Hint: %s", h.Internal.Error.Hint))
		}
	}
	return strings.Join(errorStrings, "\r\n")
}

// JSON will return the raw JSON object returned from the server
func (h *APIError) JSON() string {
	data, err := json.Marshal(&h)
	if err != nil {
		return fmt.Sprintf("error unmarshalling json error message: %v", err)
	}
	return string(data)
}
