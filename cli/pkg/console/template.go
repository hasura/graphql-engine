package console

import (
	"embed"
	"fmt"
	"html/template"
	"regexp"

	"github.com/pkg/errors"

	"github.com/gin-gonic/contrib/renders/multitemplate"
	"github.com/hasura/graphql-engine/cli/v2/version"
)

const (
	preReleaseVersion = "v1.0-alpha"
	unversioned       = "unversioned"
	versioned         = "versioned"
)

type TemplateProvider interface {
	// BasePath will return the basepath for the tempalate directory
	BasePath() string

	// This is the template filename eg: console.html, console2.html
	TemplateFilename() string

	// DoTemplateExist returns true if an asset exists at pathk
	DoTemplateExist(path string) bool
	LoadTemplates(path string, templateNames ...string) (multitemplate.Render, error)

	// GetTemplateVersion returns the template version tv required to render
	// the console html.
	GetTemplateVersion(v *version.Version) string
	// GetAssetsVersion returns the assets version av to be used in the
	// console template. This function is supposed to return the following:
	// > input           -> output
	// > dev-build       -> versioned/dev-build
	// > v1.0.0-beta.01  -> beta/v1.0
	// > v1.0.0-alpha.01 -> alpha/v1.0
	// > v1.2.1-rc.03    -> rc/v1.2
	// > v1.1.0          -> stable/v1.1
	GetAssetsVersion(v *version.Version) string
	GetAssetsCDN() string
}

// DefaultTemplateProvider implements the github.com/hasura/graphl-engine/cli/pkg/templates.DefaultTemplateProvider interface
type DefaultTemplateProvider struct {
	basePath         string
	templateFileName string
	consoleFS        embed.FS
}

func NewDefaultTemplateProvider(basePath, templateFilename string, consoleFS embed.FS) *DefaultTemplateProvider {
	return &DefaultTemplateProvider{
		basePath:         basePath,
		templateFileName: templateFilename,
		consoleFS:        consoleFS,
	}
}

func (p *DefaultTemplateProvider) BasePath() string {
	return p.basePath
}

func (p *DefaultTemplateProvider) TemplateFilename() string {
	return p.templateFileName
}

// DoTemplateExist returns true if an asset exists at pathk
func (p *DefaultTemplateProvider) DoTemplateExist(path string) bool {
	_, err := p.consoleFS.ReadFile(path)
	return err == nil
}

func (p *DefaultTemplateProvider) LoadTemplates(path string, templateNames ...string) (multitemplate.Render, error) {
	r := multitemplate.New()

	for _, templateName := range templateNames {
		templatePath := path + templateName
		templateBytes, err := p.consoleFS.ReadFile(templatePath)
		if err != nil {
			return nil, errors.Wrap(err, "error reading from file "+templatePath)
		}
		theTemplate, err := template.New(templateName).Parse(string(templateBytes))
		if err != nil {
			return nil, errors.Wrap(err, "error creating template"+path+templateName)
		}
		r.Add(templateName, theTemplate)
	}

	return r, nil
}

// GetTemplateVersion returns the template version tv required to render
// the console html.
func (p *DefaultTemplateProvider) GetTemplateVersion(v *version.Version) string {
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

// GetAssetsVersion returns the assets version av to be used in the
// console template. This function is supposed to return the following:
// > input           -> output
// > dev-build       -> versioned/dev-build
// > v1.0.0-beta.01  -> beta/v1.0
// > v1.0.0-alpha.01 -> alpha/v1.0
// > v1.2.1-rc.03    -> rc/v1.2
// > v1.1.0          -> stable/v1.1
func (p *DefaultTemplateProvider) GetAssetsVersion(v *version.Version) string {
	// server has a version
	if v.Server != "" {
		// version is semver
		if v.ServerSemver != nil {
			// check for release channels
			preRelease := v.ServerSemver.Prerelease()
			channel := "stable"
			if preRelease != "" {
				var re = regexp.MustCompile(`^[a-z]+`)
				tag := re.FindString(preRelease)
				// cloud and pro will be tagged like v2.0.0-cloud.9
				// so, tag will be set as cloud/pro
				// then assets should be loaded from stable channel
				if tag != "" && tag != "cloud" && tag != "pro" {
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

func (p *DefaultTemplateProvider) GetAssetsCDN() string {
	return "https://graphql-engine-cdn.hasura.io/console/assets"
}
