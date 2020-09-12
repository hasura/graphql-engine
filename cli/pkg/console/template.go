package console

import (
	"fmt"
	"html/template"
	"io/ioutil"
	"regexp"

	"github.com/pkg/errors"

	"github.com/gin-gonic/contrib/renders/multitemplate"
	_ "github.com/hasura/graphql-engine/cli/pkg/console/templates/packed"
	"github.com/hasura/graphql-engine/cli/version"
	"github.com/markbates/pkger"
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
}

// DefaultTemplateProvider implements the github.com/hasura/graphl-engine/cli/pkg/templates.DefaultTemplateProvider interface
type DefaultTemplateProvider struct {
	basePath         string
	templateFileName string
}

func NewDefaultTemplateProvider(basePath, templateFilename string) *DefaultTemplateProvider {
	return &DefaultTemplateProvider{
		basePath:         basePath,
		templateFileName: templateFilename,
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
	_, err := pkger.Stat(path)
	return err == nil
}

func (p *DefaultTemplateProvider) LoadTemplates(path string, templateNames ...string) (multitemplate.Render, error) {
	r := multitemplate.New()

	for _, templateName := range templateNames {
		templateFile, err := pkger.Open(path + templateName)
		if err != nil {
			return nil, errors.Wrap(err, "error opening file "+path+templateName)
		}
		templateBytes, err := ioutil.ReadAll(templateFile)
		if err != nil {
			return nil, errors.Wrap(err, "error reading from file "+path+templateName)
		}

		theTemplate, err := template.New(templateName).Parse(string(templateBytes))
		if err != nil {
			return nil, errors.Wrap(err, "error creating template"+path+templateName)
		}
		err = templateFile.Close()
		if err != nil {
			return nil, err
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
