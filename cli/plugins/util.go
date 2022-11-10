package plugins

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/tree/master/internal
*/

import (
	stderrors "errors"
	"fmt"
	"io/fs"
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"syscall"

	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/plugins/download"
)

const (
	sha256Pattern = `^[a-f0-9]{64}$`
)

var (
	safePluginRegexp = regexp.MustCompile(`^[\w-]+$`)
	validSHA256      = regexp.MustCompile(sha256Pattern)

	// windowsForbidden is taken from  https://docs.microsoft.com/en-us/windows/desktop/FileIO/naming-a-file
	windowsForbidden = []string{"CON", "PRN", "AUX", "NUL", "COM1", "COM2",
		"COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9", "LPT1", "LPT2",
		"LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9"}
)

func isValidSHA256(s string) bool { return validSHA256.MatchString(s) }

// ensureDirs makes sure the paths created
func ensureDirs(paths ...string) error {
	var op errors.Op = "plugins.ensureDirs"
	for _, p := range paths {
		if err := os.MkdirAll(p, 0755); err != nil {
			return errors.E(op, fmt.Errorf("failed to ensure create directory %q: %w", p, err))
		}
	}
	return nil
}

// IsSafePluginName checks if the plugin Name is safe to use.
func IsSafePluginName(name string) bool {
	if !safePluginRegexp.MatchString(name) {
		return false
	}
	for _, forbidden := range windowsForbidden {
		if strings.EqualFold(forbidden, name) {
			return false
		}
	}
	return true
}

// validatePlatform checks Platform for structural validity.
func validatePlatform(p Platform) error {
	var op errors.Op = "plugins.validatePlatform"
	if p.URI == "" {
		return errors.E(op, "`uri` has to be set")
	}
	if p.Sha256 == "" {
		return errors.E(op, "`sha256` sum has to be set")
	}
	if !isValidSHA256(p.Sha256) {
		return errors.E(op, fmt.Errorf("`sha256` value %s is not valid, must match pattern %s", p.Sha256, sha256Pattern))
	}
	if p.Bin == "" {
		return errors.E(op, "`bin` has to be set")
	}
	if err := validateFiles(p.Files); err != nil {
		return errors.E(op, fmt.Errorf("`files` is invalid: %w", err))
	}
	return nil
}

func validateFiles(fops []FileOperation) error {
	var op errors.Op = "plugins.validateFiles"
	if fops == nil {
		return nil
	}
	if len(fops) == 0 {
		return errors.E(op, "`files` has to be unspecified or non-empty")
	}
	for _, fop := range fops {
		if fop.From == "" {
			return errors.E(op, "`from` field has to be set")
		} else if fop.To == "" {
			return errors.E(op, "`to` field has to be set")
		}
	}
	return nil
}

// matchPlatform returns the first matching platform to given os/arch.
func MatchPlatform(platforms []Platform) (Platform, bool, error) {
	currSelector := fmt.Sprintf("%s-%s", runtime.GOOS, runtime.GOARCH)

	for _, platform := range platforms {
		if platform.Selector == currSelector {
			return platform, true, nil
		}
	}
	return Platform{}, false, nil
}

// downloadAndExtract downloads the specified archive uri (or uses the provided overrideFile, if a non-empty value)
// while validating its checksum with the provided sha256sum, and extracts its contents to extractDir that must be.
// created.
func downloadAndExtract(extractDir, uri, sha256sum string) error {
	var op errors.Op = "plugins.downloadAndExtract"
	nurl, err := url.Parse(uri)
	if err != nil {
		return errors.E(op, fmt.Errorf("unable to parse uri: %w", err))
	}
	var fetcher download.Fetcher
	if nurl.Scheme == "file" {
		fetcher = download.NewFileFetcher(nurl.Path)
	} else {
		fetcher = download.HTTPFetcher{}
	}
	verifier := download.NewSha256Verifier(sha256sum)
	err = download.NewDownloader(verifier, fetcher).Get(uri, extractDir)
	if err != nil {
		return errors.E(op, fmt.Errorf("failed to unpack the plugin archive: %w", err))
	}
	return nil
}

// IsWindows sees runtime.GOOS to find out if current execution mode is win32.
func IsWindows() bool {
	goos := runtime.GOOS
	return goos == "windows"
}

func createOrUpdateLink(binDir, binary, plugin string) error {
	var op errors.Op = "plugins.createOrUpdateLink"
	dst := filepath.Join(binDir, PluginNameToBin(plugin, IsWindows()))

	if err := removeLink(dst); err != nil {
		return errors.E(op, fmt.Errorf("failed to remove old symlink: %w", err))
	}
	if _, err := os.Stat(binary); stderrors.Is(err, fs.ErrNotExist) {
		return errors.E(op, fmt.Errorf("can't create symbolic link, source binary (%q) cannot be found in extracted archive: %w", binary, err))
	}

	// Create new
	if err := os.Symlink(binary, dst); err != nil {
		if IsWindows() {
			// If cloning the symlink fails on Windows because the user
			// does not have the required privileges, ignore the error and
			// fall back to copying the file contents.
			//
			// ERROR_PRIVILEGE_NOT_HELD is 1314 (0x522):
			// https://msdn.microsoft.com/en-us/library/windows/desktop/ms681385(v=vs.85).aspx
			var lerr *os.LinkError
			if stderrors.As(err, &lerr); lerr.Err != syscall.Errno(1314) {
				return errors.E(op, err)
			}
			if err := copyFile(binary, dst, 0755); err != nil {
				return errors.E(op, err)
			}
		} else {
			return errors.E(op, fmt.Errorf("failed to create a symlink from %q to %q: %w", binary, dst, err))
		}
	}
	return nil
}

// removeLink removes a symlink reference if exists.
func removeLink(path string) error {
	var op errors.Op = "plugins.removeLink"
	fi, err := os.Lstat(path)
	if stderrors.Is(err, fs.ErrNotExist) {
		return nil
	} else if err != nil {
		return errors.E(op, fmt.Errorf("failed to read the symlink in %q: %w", path, err))
	}

	if fi.Mode()&os.ModeSymlink == 0 && !IsWindows() {
		return errors.E(op, fmt.Errorf("file %q is not a symlink (mode=%s)", path, fi.Mode()))
	}
	if err := os.Remove(path); err != nil {
		return errors.E(op, fmt.Errorf("failed to remove the symlink in %q: %w", path, err))
	}
	return nil
}

// PluginNameToBin creates the name of the symlink file for the plugin name.
// It converts dashes to underscores.
func PluginNameToBin(name string, isWindows bool) string {
	name = strings.ReplaceAll(name, "-", "_")
	name = "hasura-" + name
	if isWindows {
		name += ".exe"
	}
	return name
}

// IsSubPath checks if the extending path is an extension of the basePath, it will return the extending path
// elements. Both paths have to be absolute or have the same root directory. The remaining path elements
func IsSubPath(basePath, subPath string) (string, bool) {
	extendingPath, err := filepath.Rel(basePath, subPath)
	if err != nil {
		return "", false
	}
	if strings.HasPrefix(extendingPath, "..") {
		return "", false
	}
	return extendingPath, true
}

// ReplaceBase will return a replacement path with replacement as a base of the path instead of the old base. a/b/c, a, d -> d/b/c
func ReplaceBase(path, old, replacement string) (string, error) {
	var op errors.Op = "plugins.ReplaceBase"
	extendingPath, ok := IsSubPath(old, path)
	if !ok {
		return "", errors.E(op, fmt.Errorf("can't replace %q in %q, it is not a subpath", old, path))
	}
	return filepath.Join(replacement, extendingPath), nil
}
