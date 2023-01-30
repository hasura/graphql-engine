package update

// shamelessly copied from
// https://github.com/inconshreveable/go-update/blob/master/hide_windows.go

import (
	"syscall"
	"unsafe"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
)

func hideFile(path string) error {
	var op errors.Op = "update.hideFile"
	kernel32 := syscall.NewLazyDLL("kernel32.dll")
	setFileAttributes := kernel32.NewProc("SetFileAttributesW")

	r1, _, err := setFileAttributes.Call(uintptr(unsafe.Pointer(syscall.StringToUTF16Ptr(path))), 2)

	if r1 == 0 {
		return errors.E(op, err)
	} else {
		return nil
	}
}
