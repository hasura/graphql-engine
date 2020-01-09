package validation

import (
	"regexp"
	"strings"

	"github.com/Masterminds/semver"
	"github.com/hasura/graphql-engine/cli/plugins/types"
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

// ValidatePlugin checks for structural validity of the Plugin object with given
// name.
func ValidatePlugin(name string, p types.Plugin) error {
	if !IsSafePluginName(name) {
		return errors.Errorf("the plugin name %q is not allowed, must match %q", name, safePluginRegexp.String())
	}
	if p.Name != name {
		return errors.Errorf("plugin should be named %q, not %q", name, p.Name)
	}
	if p.ShortDescription == "" {
		return errors.New("should have a short description")
	}
	if strings.ContainsAny(p.ShortDescription, "\r\n") {
		return errors.New("should not have line breaks in short description")
	}
	if len(p.Platforms) == 0 {
		return errors.New("should have a platform specified")
	}
	if p.Version == "" {
		return errors.New("should have a version specified")
	}
	if _, err := semver.NewVersion(p.Version); err != nil {
		return errors.Wrap(err, "failed to parse plugin version")
	}
	for _, pl := range p.Platforms {
		if err := validatePlatform(pl); err != nil {
			return errors.Wrapf(err, "platform (%+v) is badly constructed", pl)
		}
	}
	return nil
}

// validatePlatform checks Platform for structural validity.
func validatePlatform(p types.Platform) error {
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
	/*
		if err := validateSelector(p.Selector); err != nil {
			return errors.Wrap(err, "invalid platform selector")
		}
	*/
	return nil
}

func validateFiles(fops []types.FileOperation) error {
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
