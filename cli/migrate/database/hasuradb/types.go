package hasuradb

import (
	"encoding/json"
	"fmt"
	"strings"

	log "github.com/sirupsen/logrus"
)

const (
	TuplesOK  = "TuplesOk"
	CommandOK = "CommandOk"
)

type HasuraInterfaceBulk struct {
	Type string        `json:"type" yaml:"type"`
	Args []interface{} `json:"args" yaml:"args"`
}

func (h *HasuraInterfaceBulk) ResetArgs() {
	h.Args = make([]interface{}, 0)
}

type HasuraInterfaceQuery struct {
	Type string      `json:"type" yaml:"type"`
	Args interface{} `json:"args" yaml:"args"`
}

type HasuraQuery struct {
	Type string     `json:"type" yaml:"type"`
	Args HasuraArgs `json:"args" yaml:"args"`
}

type HasuraBulk struct {
	Type string        `json:"type" yaml:"type"`
	Args []HasuraQuery `json:"args" yaml:"args"`
}

type HasuraArgs struct {
	SQL       string        `json:"sql,omitempty" yaml:"sql"`
	Table     interface{}   `json:"table,omitempty"`
	Columns   interface{}   `json:"columns,omitempty"`
	Where     interface{}   `json:"where,omitempty"`
	OrderBy   interface{}   `json:"order_by,omitempty"`
	Objects   []interface{} `json:"objects,omitempty"`
	Limit     int           `json:"limit,omitempty"`
	Returning []string      `json:"returning,omitempty"`
	Set       interface{}   `json:"$set,omitempty"`
}

type HasuraOrderBy struct {
	Column string `json:"column,omitempty"`
	Type   string `json:"type,omitempty"`
	Nulls  string `json:"nulls,omitempty"`
}

type HasuraColumn struct {
	Name    string      `json:"name"`
	Columns interface{} `json:"columns,omitempty"`
}

type HasuraError struct {
	// MigrationFile is used internally for hasuractl
	migrationFile  string
	migrationQuery string
	Path           string            `json:"path"`
	ErrorMessage   string            `json:"error"`
	Internal       *SQLInternalError `json:"internal,omitempty"`
	Message        string            `json:"message,omitempty"`
	Code           string            `json:"code"`
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

type SchemaDump struct {
	Opts        []string `json:"opts"`
	CleanOutput bool     `json:"clean_output"`
}

func (h *HasuraError) CMDError() error {
	var errorStrings []string
	errorStrings = append(errorStrings, fmt.Sprintf("[%s] %s (%s)", h.Code, h.ErrorMessage, h.Path))
	if h.migrationFile != "" {
		errorStrings = append(errorStrings, fmt.Sprintf("File: '%s'", h.migrationFile))
	}
	if h.migrationQuery != "" {
		errorStrings = append(errorStrings, fmt.Sprintf("%s", h.migrationQuery))
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
	return fmt.Errorf(strings.Join(errorStrings, "\r\n"))
}

func (h *HasuraError) APIError() error {
	data, err := json.Marshal(&h)
	if err != nil {
		return err
	}
	return fmt.Errorf("Data Error: %s", string(data))
}

func (h *HasuraError) Error(isCMD bool) error {
	var err error
	switch isCMD {
	case true:
		err = h.CMDError()
	case false:
		err = h.APIError()
	}
	log.Debug(err)
	return err
}

type HasuraSQLRes struct {
	ResultType string     `json:"result_type"`
	Result     [][]string `json:"result"`
}
