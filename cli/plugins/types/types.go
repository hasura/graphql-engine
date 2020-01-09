package types

// Plugin describes a plugin manifest file.
type Plugin struct {
	Name             string     `json:"name,omitempty"`
	Version          string     `json:"version,omitempty"`
	ShortDescription string     `json:"shortDescription,omitempty"`
	Homepage         string     `json:"homepage,omitempty"`
	Platforms        []Platform `json:"platforms,omitempty"`
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
