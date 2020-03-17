package console

import (
	"net/http"

	"github.com/gin-contrib/static"
	"github.com/gin-gonic/gin"
	"github.com/hasura/graphql-engine/cli/pkg/templates"
	"github.com/pkg/errors"
)

func BuildConsoleRouter(templateProvider templates.Provider, assetsVersion, staticDir string, opts gin.H) (*gin.Engine, error) {
	// An Engine instance with the Logger and Recovery middleware already attached.
	r := gin.New()

	if !templateProvider.DoAssetExist(templateProvider.BasePath() + assetsVersion + "/console.html") {
		assetsVersion = "latest"
	}

	// Template console.html
	templateRender, err := templateProvider.LoadTemplates(templateProvider.BasePath()+assetsVersion+"/", "console.html")
	if err != nil {
		return nil, errors.Wrap(err, "cannot fetch template")
	}
	r.HTMLRender = templateRender

	if staticDir != "" {
		r.Use(static.Serve("/static", static.LocalFile(staticDir, false)))
		opts["cliStaticDir"] = staticDir
	}
	r.GET("/*action", func(c *gin.Context) {
		c.HTML(http.StatusOK, "console.html", &opts)
	})

	return r, nil
}
