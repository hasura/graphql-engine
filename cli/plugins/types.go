package plugins

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/tree/master/internal
*/

import (
	"strings"

	"github.com/pkg/errors"
)

// Plugin describes a plugin manifest file.
type Plugin struct {
	Name             string     `json:"name,omitempty"`
	Version          string     `json:"version,omitempty"`
	ShortDescription string     `json:"shortDescription,omitempty"`
	Homepage         string     `json:"homepage,omitempty"`
	Hidden           bool       `json:"hidden,omitempty"`
	Platforms        []Platform `json:"platforms,omitempty"`
}

// ValidatePlugin checks for structural validity of the Plugin object with given
// name.
func (p Plugin) ValidatePlugin(name string) error {
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
	for _, pl := range p.Platforms {
		if err := validatePlatform(pl); err != nil {
			return errors.Wrapf(err, "platform (%+v) is badly constructed", pl)
		}
	}
	return nil
}

// Platform describes how to perform an installation on a specific platform
// and how to match the target platform (os, arch).
type Platform struct {
	URI      string          `json:"uri,omitempty"`
	Sha256   string          `json:"sha256,omitempty"`
	Files    []FileOperation `json:"files"`
	Selector string          `json:"selector"`
	// Bin specifies the path to the plugin executable.
	// The path is relative to the root of the installation folder.
	// The binary will be linked after all FileOperations are executed.
	Bin string `json:"bin"`
}

// FileOperation specifies a file copying operation from plugin archive to the
// installation directory.
type FileOperation struct {
	From string `json:"from,omitempty"`
	To   string `json:"to,omitempty"`
}
