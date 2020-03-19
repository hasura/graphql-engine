package templates

import (
	"github.com/gin-gonic/contrib/renders/multitemplate"
	"github.com/hasura/graphql-engine/cli/version"
)

type Provider interface {
	// BasePath will return the basepath for the tempalate directory
	BasePath() string

	// This is the template filename eg: console.html, console2.html
	TemplateFilename() string

	// DoAssetExist returns true if an asset exists at pathk
	DoAssetExist(path string) bool
	LoadTemplates(path string, templateNames ...string) (multitemplate.Render, error)

	// GetConsoleTemplateVersion returns the template version tv required to render
	// the console html.
	GetConsoleTemplateVersion(v *version.Version) string
	// GetConsoleAssetsVersion returns the assets version av to be used in the
	// console template. This function is supposed to return the following:
	// > input           -> output
	// > dev-build       -> versioned/dev-build
	// > v1.0.0-beta.01  -> beta/v1.0
	// > v1.0.0-alpha.01 -> alpha/v1.0
	// > v1.2.1-rc.03    -> rc/v1.2
	// > v1.1.0          -> stable/v1.1
	GetConsoleAssetsVersion(v *version.Version) string
}
