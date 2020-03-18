package oss

import (
	"fmt"
	"html/template"
	"regexp"

	"github.com/gin-gonic/contrib/renders/multitemplate"
	"github.com/hasura/graphql-engine/cli/pkg/templates/oss/packed"
	"github.com/hasura/graphql-engine/cli/version"
)

const (
	preReleaseVersion = "v1.0-alpha"
	unversioned       = "unversioned"
	versioned         = "versioned"
)

// Provider implemets the github.com/hasura/graphl-engine/cli/pkg/templates.Provider interface
type Provider struct {
	basePath string
}

func NewOSSProvider(basePath string) *Provider {
	return &Provider{
		basePath: basePath,
	}
}

func (p *Provider) BasePath() string {
	return p.basePath
}

// DoAssetExist returns true if an asset exists at pathk
func (p *Provider) DoAssetExist(path string) bool {
	_, err := packed.AssetInfo(path)
	return err == nil
}

func (p *Provider) LoadTemplates(path string, templateNames ...string) (multitemplate.Render, error) {
	r := multitemplate.New()

	for _, templateName := range templateNames {
		templateBytes, err := packed.Asset(path + templateName)
		if err != nil {
			return nil, err
		}

		template, err := template.New(templateName).Parse(string(templateBytes))
		if err != nil {
			return nil, err
		}

		r.Add(templateName, template)
	}

	return r, nil
}

// GetConsoleTemplateVersion returns the template version tv required to render
// the console html.
func (p *Provider) GetConsoleTemplateVersion(v *version.Version) string {
	// pre-release builds
	if v.Server == "" {
		return preReleaseVersion
	}
	// tagged build
	if v.Server != "" {
		if v.ServerSemver != nil {
			return fmt.Sprintf("v%d.%d", v.ServerSemver.Major(), v.ServerSemver.Minor())
		}
	}
	// untagged version
	return unversioned
}

// GetConsoleAssetsVersion returns the assets version av to be used in the
// console template. This function is supposed to return the following:
// > input           -> output
// > dev-build       -> versioned/dev-build
// > v1.0.0-beta.01  -> beta/v1.0
// > v1.0.0-alpha.01 -> alpha/v1.0
// > v1.2.1-rc.03    -> rc/v1.2
// > v1.1.0          -> stable/v1.1
func (p *Provider) GetConsoleAssetsVersion(v *version.Version) string {
	// server has a version
	if v.Server != "" {
		// version is semver
		if v.ServerSemver != nil {
			// check for release channels
			preRelease := v.ServerSemver.Prerelease()
			channel := "stable"
			if preRelease != "" {
				// Get the correct channel from the prerelease tag
				var re = regexp.MustCompile(`^[a-z]+`)
				tag := re.FindString(preRelease)
				if tag != "" {
					channel = tag
				}
			}
			return fmt.Sprintf("channel/%s/v%d.%d", channel, v.ServerSemver.Major(), v.ServerSemver.Minor())
		}
		// version is not semver
		return fmt.Sprintf("%s/%s", versioned, v.Server)
	}
	// server doesn't have a version - very old server :(
	return preReleaseVersion
}
