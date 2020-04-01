package util

import (
	"html/template"
	"net/http"
	"strings"

	assetfs "github.com/elazarl/go-bindata-assetfs"
	"github.com/gin-gonic/contrib/renders/multitemplate"
	"github.com/hasura/graphql-engine/cli/assets"
)

type binaryFileSystem struct {
	fs http.FileSystem
}

func (b *binaryFileSystem) Open(name string) (http.File, error) {
	return b.fs.Open(name)
}

func (b *binaryFileSystem) Exists(prefix string, filepath string) bool {

	if p := strings.TrimPrefix(filepath, prefix); len(p) < len(filepath) {
		if _, err := b.fs.Open(p); err != nil {
			return false
		}
		return true
	}
	return false
}

// BinaryFileSystem creates a binary file system at root from the assets
func BinaryFileSystem(root string) *binaryFileSystem {
	fs := &assetfs.AssetFS{assets.Asset, assets.AssetDir, assets.AssetInfo, root}
	return &binaryFileSystem{
		fs,
	}
}

// LoadTemplates loads templates from path for the given list
func LoadTemplates(path string, list ...string) (multitemplate.Render, error) {
	r := multitemplate.New()

	for _, x := range list {
		templateString, err := assets.Asset(path + x)
		if err != nil {
			return nil, err
		}

		tmplMessage, err := template.New(x).Parse(string(templateString))
		if err != nil {
			return nil, err
		}

		r.Add(x, tmplMessage)
	}

	return r, nil
}

// DoAssetExist returns true if an asset exists at pathk
func DoAssetExist(path string) bool {
	_, err := assets.AssetInfo(path)
	if err != nil {
		return false
	}
	return true
}
