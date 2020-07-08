package plugins

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/tree/master/internal
*/

import (
	"fmt"
	"net/url"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"syscall"

	"github.com/hasura/graphql-engine/cli/plugins/download"
	"github.com/pkg/errors"
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
	for _, p := range paths {
		if err := os.MkdirAll(p, 0755); err != nil {
			return errors.Wrapf(err, "failed to ensure create directory %q", p)
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
	if p.URI == "" {
		return errors.New("`uri` has to be set")
	}
	if p.Sha256 == "" {
		return errors.New("`sha256` sum has to be set")
	}
	if !isValidSHA256(p.Sha256) {
		return errors.Errorf("`sha256` value %s is not valid, must match pattern %s", p.Sha256, sha256Pattern)
	}
	if p.Bin == "" {
		return errors.New("`bin` has to be set")
	}
	if err := validateFiles(p.Files); err != nil {
		return errors.Wrap(err, "`files` is invalid")
	}
	return nil
}

func validateFiles(fops []FileOperation) error {
	if fops == nil {
		return nil
	}
	if len(fops) == 0 {
		return errors.New("`files` has to be unspecified or non-empty")
	}
	for _, op := range fops {
		if op.From == "" {
			return errors.New("`from` field has to be set")
		} else if op.To == "" {
			return errors.New("`to` field has to be set")
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
	nurl, err := url.Parse(uri)
	if err != nil {
		return errors.Wrap(err, "unable to parse uri")
	}
	var fetcher download.Fetcher
	if nurl.Scheme == "file" {
		fetcher = download.NewFileFetcher(nurl.Path)
	} else {
		fetcher = download.HTTPFetcher{}
	}
	verifier := download.NewSha256Verifier(sha256sum)
	err = download.NewDownloader(verifier, fetcher).Get(uri, extractDir)
	return errors.Wrap(err, "failed to unpack the plugin archive")
}

// IsWindows sees runtime.GOOS to find out if current execution mode is win32.
func IsWindows() bool {
	goos := runtime.GOOS
	return goos == "windows"
}

func createOrUpdateLink(binDir, binary, plugin string) error {
	dst := filepath.Join(binDir, PluginNameToBin(plugin, IsWindows()))

	if err := removeLink(dst); err != nil {
		return errors.Wrap(err, "failed to remove old symlink")
	}
	if _, err := os.Stat(binary); os.IsNotExist(err) {
		return errors.Wrapf(err, "can't create symbolic link, source binary (%q) cannot be found in extracted archive", binary)
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
			if lerr, ok := err.(*os.LinkError); ok && lerr.Err != syscall.Errno(1314) {
				return err
			}
			if err := copyFile(binary, dst, 0755); err != nil {
				return err
			}
		} else {
			return errors.Wrapf(err, "failed to create a symlink from %q to %q", binary, dst)
		}
	}
	return nil
}

// removeLink removes a symlink reference if exists.
func removeLink(path string) error {
	fi, err := os.Lstat(path)
	if os.IsNotExist(err) {
		return nil
	} else if err != nil {
		return errors.Wrapf(err, "failed to read the symlink in %q", path)
	}

	if fi.Mode()&os.ModeSymlink == 0 && !IsWindows() {
		return errors.Errorf("file %q is not a symlink (mode=%s)", path, fi.Mode())
	}
	if err := os.Remove(path); err != nil {
		return errors.Wrapf(err, "failed to remove the symlink in %q", path)
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
	extendingPath, ok := IsSubPath(old, path)
	if !ok {
		return "", errors.Errorf("can't replace %q in %q, it is not a subpath", old, path)
	}
	return filepath.Join(replacement, extendingPath), nil
}
