// +build !windows

package update

// shamelessly copied from
// https://github.com/inconshreveable/go-update/blob/master/hide_windows.go

func hideFile(path string) error {
	return nil
}
