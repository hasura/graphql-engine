package util

import (
	"github.com/Masterminds/semver"
)

type VersionFlag struct {
	Version *semver.Version
}

// NewVersionFlagValue returns ConfigVersion set with default value
func NewVersionFlagValue(p *VersionFlag) *VersionFlag {
	return p
}

// Set sets the value of the named command-line flag.
func (c *VersionFlag) Set(s string) error {
	v, err := semver.NewVersion(s)
	if err != nil {
		return err
	}
	c.Version = v
	return nil
}

// Type returns a string that uniquely represents this flag's type.
func (c *VersionFlag) Type() string {
	return "string"
}

func (c *VersionFlag) String() string {
	if c.Version == nil {
		return ""
	}
	return c.Version.String()
}
