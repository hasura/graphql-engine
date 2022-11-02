package hasuradb

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/mitchellh/mapstructure"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

type HasuraInterfaceBulk struct {
	Type string        `json:"type" yaml:"type"`
	Args []interface{} `json:"args" yaml:"args"`
}

func (h *HasuraInterfaceBulk) ResetArgs() {
	h.Args = make([]interface{}, 0)
}

type HasuraInterfaceQuery struct {
	Type    string      `json:"type" yaml:"type"`
	Version int         `json:"version,omitempty" yaml:"version,omitempty"`
	Source  string      `json:"source,omitempty" yaml:"source,omitempty"`
	Args    interface{} `json:"args" yaml:"args"`
}

type HasuraError struct {
	// MigrationFile is used internally for hasuractl
	migrationFile  string
	migrationQuery string
	Path           string      `json:"path"`
	ErrorMessage   string      `json:"error"`
	Internal       interface{} `json:"internal,omitempty"`
	Message        string      `json:"message,omitempty"`
	Code           string      `json:"code"`
}

type InconsistentMetadataError struct {
	Definition interface{} `json:"definition,omitempty" mapstructure:"definition,omitempty"`
	Reason     string      `json:"reason,omitempty" mapstructure:"reason,omitempty"`
	Type       string      `json:"type,omitempty" mapstructure:"type,omitempty"`
}

func (mderror *InconsistentMetadataError) String() string {
	var out string
	if mderror.Reason != "" {
		out = fmt.Sprintf("\nreason: %v\n", mderror.Reason)
	}
	if mderror.Type != "" {
		out = fmt.Sprintf("%stype: %v\n", out, mderror.Type)
	}
	if mderror.Definition != nil {
		m, err := json.MarshalIndent(mderror.Definition, "", "  ")
		if err == nil {
			out = fmt.Sprintf("%sdefinition: \n%s", out, string(m))
		}
	}
	return out
}

type SQLInternalError struct {
	Arguments                 []string       `json:"arguments" mapstructure:"arguments,omitempty"`
	Error                     *PostgresError `json:"error" mapstructure:"error,omitempty"`
	Prepared                  bool           `json:"prepared" mapstructure:"prepared,omitempty"`
	Statement                 string         `json:"statement" mapstructure:"statement,omitempty"`
	InconsistentMetadataError `mapstructure:",squash"`
}
type PostgresError struct {
	StatusCode  string `json:"status_code" mapstructure:"status_code,omitempty"`
	ExecStatus  string `json:"exec_status" mapstructure:"exec_status,omitempty"`
	Message     string `json:"message" mapstructure:"message,omitempty"`
	Description string `json:"description" mapstructure:"description,omitempty"`
	Hint        string `json:"hint" mapstructure:"hint,omitempty"`
}

type SchemaDump struct {
	Opts        []string `json:"opts"`
	CleanOutput bool     `json:"clean_output"`
	Database    string   `json:"source,omitempty"`
}

func (h HasuraError) Error() string {
	var errorStrings []string
	errorStrings = append(errorStrings, fmt.Sprintf("[%s] %s (%s)", h.Code, h.ErrorMessage, h.Path))
	if h.migrationFile != "" {
		errorStrings = append(errorStrings, fmt.Sprintf("File: '%s'", h.migrationFile))
	}
	if h.migrationQuery != "" {
		errorStrings = append(errorStrings, h.migrationQuery)
	}
	var internalError SQLInternalError
	var internalErrors []SQLInternalError
	if v, ok := h.Internal.(map[string]interface{}); ok {
		err := mapstructure.Decode(v, &internalError)
		if err == nil {
			// postgres error
			if internalError.Error != nil {
				errorStrings = append(errorStrings, fmt.Sprintf("[%s] %s: %s", internalError.Error.StatusCode, internalError.Error.ExecStatus, internalError.Error.Message))
				if len(internalError.Error.Description) > 0 {
					errorStrings = append(errorStrings, fmt.Sprintf("Description: %s", internalError.Error.Description))
				}
				if len(internalError.Error.Hint) > 0 {
					errorStrings = append(errorStrings, fmt.Sprintf("Hint: %s", internalError.Error.Hint))
				}
			}
			if e := internalError.InconsistentMetadataError.String(); e != "" {
				errorStrings = append(errorStrings, e)
			}
		}
	}
	if v, ok := h.Internal.([]interface{}); ok {
		err := mapstructure.Decode(v, &internalErrors)
		if err == nil {
			for _, internalError := range internalErrors {
				// postgres error
				if internalError.Error != nil {
					errorStrings = append(errorStrings, fmt.Sprintf("[%s] %s: %s", internalError.Error.StatusCode, internalError.Error.ExecStatus, internalError.Error.Message))
					if len(internalError.Error.Description) > 0 {
						errorStrings = append(errorStrings, fmt.Sprintf("Description: %s", internalError.Error.Description))
					}
					if len(internalError.Error.Hint) > 0 {
						errorStrings = append(errorStrings, fmt.Sprintf("Hint: %s", internalError.Error.Hint))
					}
				}

				if e := internalError.InconsistentMetadataError.String(); e != "" {
					errorStrings = append(errorStrings, e)
				}
			}
		}
	}
	if len(errorStrings) == 0 {
		return ""
	}
	return strings.Join(errorStrings, "\r\n")
}

// NewHasuraError - returns error based on data and isCmd
func NewHasuraError(data []byte, isCmd bool) error {
	var op errors.Op = "hasuradb.NewHasuraError"
	switch isCmd {
	case true:
		var herror HasuraError
		err := json.Unmarshal(data, &herror)
		if err != nil {
			return errors.E(op, fmt.Errorf("failed parsing json: %w; response from API: %s", err, string(data)))
		}
		return herror
	default:
		return errors.E(op, fmt.Errorf("Data Error: %s", string(data)))
	}
}
