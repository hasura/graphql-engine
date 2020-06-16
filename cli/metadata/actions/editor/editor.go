package editor

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

const (
	defaultEditor = "vi"
	defaultShell  = "/bin/bash"
	windowsEditor = "notepad"
	windowsShell  = "cmd"
)

// PreferredEditorResolver is a function that returns an editor that the user
// prefers to use, such as the configured `$EDITOR` environment variable.
type PreferredEditorResolver func() ([]string, bool)

// GetPreferredEditorFromEnvironment returns the user's editor as defined by the
// `$EDITOR` environment variable, or the `DefaultEditor` if it is not set.
func GetPreferredEditorFromEnvironment() ([]string, bool) {
	editor := os.Getenv("EDITOR")
	if len(editor) == 0 {
		editor = platformize(defaultEditor, windowsEditor)
	}

	if !strings.Contains(editor, " ") {
		return []string{editor}, false
	}

	if !strings.ContainsAny(editor, "\"'\\") {
		return strings.Split(editor, " "), false
	}

	shell := defaultEnvShell()
	return append(shell, editor), true
}

// OpenFileInEditor opens filename in a text editor.
func OpenFileInEditor(filename string, resolveEditor PreferredEditorResolver) error {
	// Get the full executable path for the editor.
	args, shell := resolveEditor()
	if len(args) == 0 {
		return fmt.Errorf("no editor defined, can't open %s", filename)
	}

	abs, err := filepath.Abs(filename)
	if err != nil {
		return err
	}

	cmdArgs := make([]string, len(args))
	copy(cmdArgs, args)
	if shell {
		last := cmdArgs[len(cmdArgs)-1]
		cmdArgs[len(cmdArgs)-1] = fmt.Sprintf("%s %q", last, abs)
	} else {
		cmdArgs = append(cmdArgs, abs)
	}

	cmd := exec.Command(cmdArgs[0], cmdArgs[1:]...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// CaptureInputFromEditor opens a temporary file in a text editor and returns
// the written bytes on success or an error on failure. It handles deletion
// of the temporary file behind the scenes.
func CaptureInputFromEditor(resolveEditor PreferredEditorResolver, text, extension string) ([]byte, error) {
	file, err := ioutil.TempFile(os.TempDir(), fmt.Sprintf("*.%s", extension))
	if err != nil {
		return []byte{}, err
	}

	filename := file.Name()

	// Defer removal of the temporary file in case any of the next steps fail.
	defer os.Remove(filename)

	_, err = file.Write([]byte(text))
	if err != nil {
		return []byte{}, err
	}

	if err = file.Close(); err != nil {
		return []byte{}, err
	}

	if err = OpenFileInEditor(filename, resolveEditor); err != nil {
		return []byte{}, err
	}

	bytes, err := ioutil.ReadFile(filename)
	if err != nil {
		return []byte{}, err
	}

	return bytes, nil
}

func defaultEnvShell() []string {
	shell := os.Getenv("SHELL")
	if len(shell) == 0 {
		shell = platformize(defaultShell, windowsShell)
	}
	flag := "-c"
	if shell == windowsShell {
		flag = "/C"
	}
	return []string{shell, flag}
}

func platformize(linux, windows string) string {
	if runtime.GOOS == "windows" {
		return windows
	}
	return linux
}
