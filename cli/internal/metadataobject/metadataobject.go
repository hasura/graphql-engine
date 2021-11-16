package metadataobject

import (
	"fmt"
	"strings"

	"gopkg.in/yaml.v2"
)

type Objects []Object

type Object interface {
	Build(metadata *yaml.MapSlice) ErrParsingMetadataObject
	Export(metadata yaml.MapSlice) (map[string][]byte, ErrParsingMetadataObject)
	CreateFiles() error
	Key() string
	Filename() string
}

type ErrParsingMetadataObject interface {
	// ObjectName corresponds to metadata object in JSON format
	// eg: source, api_limits etc
	ObjectName() string
	// Filename corresponding to metadata object
	// eg: databases.yaml, actions.yaml
	Filename() string
	// ErrorContext will contain any additional information regarding error
	ErrorContext() string

	// Unwrap will make sure the error returned is unwrappable
	// https://blog.golang.org/go1.13-errors#TOC_3.1.
	Unwrap() error
	error
}

func NewErrParsingMetadataObject(o Object, err error, context ...string) ErrParsingMetadataObject {
	return &errParsingMetadataObjectFile{o.Key(), o.Filename(), context, err}
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
