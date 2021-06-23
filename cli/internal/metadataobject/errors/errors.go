package errors

import (
	"fmt"
	"strings"
)

type ErrParsingMetadataObject interface {
	// ObjectName corresponds to metadata object in JSON format
	// eg: source, api_limits etc
	ObjectName() string
	// Filename corresponding to metadata object
	// eg: databases.yaml, actions.yaml
	Filename() string
	// ErrorContext will contain any additional information regarding error
	ErrorContext() string

	Unwrap() error
	error
}

func NewErrParsingMetadataObject(objectName, filename string, context []string, err error) ErrParsingMetadataObject {
	return &errParsingMetadataObjectFile{objectName, filename, context, err}
}

type errParsingMetadataObjectFile struct {
	objectName string
	fileName   string
	context    []string
	err        error
}

func (e *errParsingMetadataObjectFile) ObjectName() string {
	return e.objectName
}

func (e *errParsingMetadataObjectFile) Filename() string {
	return e.fileName
}

func (e *errParsingMetadataObjectFile) ErrorContext() string {
	if len(e.context) != 0 {
		return fmt.Sprintf("context: %v\n", strings.Join(e.context, "\n"))
	}
	return ""
}

func (e *errParsingMetadataObjectFile) Error() string {
	return strings.Trim(fmt.Sprintf(
		"\nerror parsing metadata \nobject: %v\nfile: %v\nerror: %v\n%v",
		e.ObjectName(),
		e.Filename(),
		e.err.Error(),
		e.ErrorContext(),
	), "\n")
}

func (e *errParsingMetadataObjectFile) Unwrap() error { return e.err }
