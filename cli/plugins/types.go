package plugins

/*
some of the code here is borrowed from the krew codebse (kubernetes)
and the copyright belongs to the respective authors.

source: https://github.com/kubernetes-sigs/krew/tree/master/internal
*/

import (
	"fmt"
	"sort"
	"strings"

	"github.com/Masterminds/semver"
	"github.com/pkg/errors"
)

// Plugins - holds multiple plugins
type Plugins map[string]*PluginVersions

// PluginVersions holds manifest data for multiple versions of a plugin
type PluginVersions struct {
	Index    versionSlice
	Versions map[*semver.Version]Plugin
}

func NewPluginVersions() *PluginVersions {
	return &PluginVersions{
		Index:    make(versionSlice, 0),
		Versions: make(map[*semver.Version]Plugin),
	}
}

func (i *PluginVersions) Append(p Plugin) (err error) {
	if _, ok := i.Versions[p.ParsedVersion]; ok {
		return fmt.Errorf("found duplicate versions for plugin %s - %s", p.Name, p.Version)
	}

	i.Versions[p.ParsedVersion] = p
	i.buildIndex()
	return nil
}

func (i *PluginVersions) buildIndex() {
	i.Index = make(versionSlice, 0)
	for version := range i.Versions {
		i.Index = append(i.Index, version)
	}
	sort.Sort(i.Index)
}

// versionSlice is a collection of Version instances and implements the sort
// interface. See the sort package for more details.
// https://golang.org/pkg/sort/
type versionSlice []*semver.Version

// Len returns the length of a collection. The number of Version instances
// on the slice.
func (v versionSlice) Len() int {
	return len(v)
}

// Swap is needed for the sort interface to replace the Version objects
// at two different positions in the slice.
func (v versionSlice) Swap(i, j int) {
	v[i], v[j] = v[j], v[i]
}

// Less is needed for the sort interface to compare two Version objects on the
// slice. If checks if one is less than the other.
func (v versionSlice) Less(i, j int) bool {
	return v[i].LessThan(v[j])
}

// Search is needed to search a particular version
func (v versionSlice) Search(x *semver.Version) *semver.Version {
	for _, ver := range v {
		if ver.String() == x.String() {
			return ver
		}
	}
	return nil
}

// Plugin describes a plugin manifest file.
type Plugin struct {
	Name             string     `json:"name,omitempty"`
	Version          string     `json:"version,omitempty"`
	ShortDescription string     `json:"shortDescription,omitempty"`
	Homepage         string     `json:"homepage,omitempty"`
	Hidden           bool       `json:"hidden,omitempty"`
	Platforms        []Platform `json:"platforms,omitempty"`

	ParsedVersion *semver.Version `json:"-"`
}

// ParseVersion - ensures the version is valid
func (p *Plugin) ParseVersion() {
	v, err := semver.NewVersion(p.Version)
	if err != nil {
		p.ParsedVersion = nil
		return
	}
	p.ParsedVersion = v
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
