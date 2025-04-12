package errors

import (
	"errors"
	"fmt"
	"log"
	"runtime"

	"github.com/sirupsen/logrus"
)

var Level logrus.Level

type Error struct {
	// Op is the operation being performed
	// this can be named by convention as packageName.FunctionName
	// eg: var op = "errors.E"
	// for methods on structs this can be packageName.structName.methodName
	Op       Op
	Kind     Kind
	Err      error
	Location ErrLocation
}

type ErrLocation struct {
	File string
	Line int
}

func (l ErrLocation) String() string {
	return fmt.Sprintf("file: %v, line: %v", l.File, l.Line)
}

func (e Error) Unwrap() error {
	return e.Err
}

type Unwrapper interface {
	Unwrap() error
}

var (
	_ error     = (*Error)(nil)
	_ Unwrapper = (*Error)(nil)
)

func (e *Error) Error() string {
	return e.Err.Error()
}

var _ error = (*Error)(nil)

type Op string

type Kind uint16

type HRE string

const (
	KindOther Kind = iota + 1
	KindInternal
	KindHasuraAPI
	KindBadInput
	KindNetwork
)

func (k Kind) String() string {
	switch k {
	case KindOther:
		return "other error"
	case KindInternal:
		return "internal error"
	case KindHasuraAPI:
		return "hasura server API error"
	case KindBadInput:
		return "bad input"
	case KindNetwork:
		return "network error"
	}
	return "unknown error kind"
}

func E(op Op, args ...interface{}) error {
	_, file, line, _ := runtime.Caller(1)
	e := &Error{
		Op: op,
		Location: ErrLocation{
			File: file,
			Line: line,
		},
	}
	for _, arg := range args {
		switch arg := arg.(type) {
		case Kind:
			e.Kind = arg
		case error:
			e.Err = arg
		case string:
			e.Err = errors.New(arg)
		default:
			log.Printf("errors.E: bad call from %s:%d:%v: %v", file, line, op, args)
		}
	}
	return e
}

func IsKind(want Kind, err error) bool {
	got := GetKind(err)
	return got == want
}

func Ops(err *Error) []Op {
	ops := []Op{err.Op}
	for {
		var embeddedErr *Error
		if !errors.As(err.Err, &embeddedErr) {
			break
		}

		ops = append(ops, embeddedErr.Op)
		err = embeddedErr
	}

	return ops
}

func GetLocation(err error) ErrLocation {
	var prevErr *Error
	for {
		var e *Error
		if !errors.As(err, &e) {
			return prevErr.Location
		}
		prevErr = e
		err = e.Err
	}
}

func GetKind(err error) Kind {
	var e *Error
	if !errors.As(err, &e) {
		return KindOther
	}
	if e.Kind != 0 {
		return e.Kind
	}
	return GetKind(e.Err)
}
