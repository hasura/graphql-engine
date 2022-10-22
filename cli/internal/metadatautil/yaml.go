package metadatautil

import (
	"fmt"
	"io/ioutil"
	"path/filepath"
	"strings"

	"gopkg.in/yaml.v3"
)

const baseDirectoryKey = "base_directory"
const includeTag = "!include"

type YamlDecoderOpts struct {
	// directory which is to be used as the parent directory to look for filenames
	// specified in !include tag
	IncludeTagBaseDirectory string
}

type yamlDecoder struct {
	destination interface{}
	opts        YamlDecoderOpts
}

func NewYamlDecoder(opts YamlDecoderOpts, destination interface{}) *yamlDecoder {
	return &yamlDecoder{destination, opts}
}

func (s *yamlDecoder) UnmarshalYAML(value *yaml.Node) error {
	ctx := map[string]string{}
	ctx[baseDirectoryKey] = s.opts.IncludeTagBaseDirectory

	resolved, err := resolveTags(ctx, value, nil)
	if err != nil {
		return err
	}
	return resolved.Decode(s.destination)
}

type fragment struct {
	ctx     map[string]string
	files   *[]string
	content *yaml.Node
}

func newFragment(ctx map[string]string, files *[]string) *fragment {
	f := new(fragment)
	f.ctx = ctx
	f.files = files
	return f
}
func (f *fragment) UnmarshalYAML(value *yaml.Node) error {
	var err error
	// process includes in fragments
	f.content, err = resolveTags(f.ctx, value, f.files)
	return err
}

var resolver = func(ctx map[string]string, node *yaml.Node, files *[]string) (*yaml.Node, error) {
	if node.Kind != yaml.ScalarNode {
		return nil, fmt.Errorf("found %s on scalar node", baseDirectoryKey)
	}
	baseDir, ok := ctx[baseDirectoryKey]
	if !ok {
		return nil, fmt.Errorf("parser error: base directory for !include tag not specified")
	}
	fileLocation := filepath.Join(baseDir, node.Value)
	file, err := ioutil.ReadFile(fileLocation)
	if err != nil {
		return nil, fmt.Errorf("%s: %w", fileLocation, err)
	}
	if files != nil {
		*files = append(*files, fileLocation)
	}
	if filepath.Ext(fileLocation) != ".yaml" {
		node.Value = string(file)
		return node, nil
	}
	newctx := map[string]string{}
	for k, v := range ctx {
		newctx[k] = v
	}
	newctx[baseDirectoryKey] = filepath.Dir(filepath.Join(baseDir, node.Value))
	var f = newFragment(newctx, files)
	err = yaml.Unmarshal(file, f)
	if err != nil {
		return nil, fmt.Errorf("%s: %w", fileLocation, err)
	}
	return f.content, nil
}

func resolveTags(ctx map[string]string, node *yaml.Node, files *[]string) (*yaml.Node, error) {
	switch node.Tag {
	case includeTag:
		return resolver(ctx, node, files)
	case "!!str":
		if strings.Contains(node.Value, includeTag) {
			node.Tag = includeTag
			parts := strings.Split(node.Value, " ")
			node.Value = strings.Trim(strings.Join(parts[1:], " "), "\"")
			return resolver(ctx, node, files)
		}
	}

	switch node.Kind {
	case yaml.DocumentNode, yaml.SequenceNode, yaml.MappingNode:
		var err error
		for idx := range node.Content {
			node.Content[idx], err = resolveTags(ctx, node.Content[idx], files)
			if err != nil {
				return nil, err
			}
		}
	}
	return node, nil
}

// GetIncludeTagFiles files will return file paths of all child !include tag values
// eg:
// for example say the following are contents rootfile.yaml
// somekey: somevalue
// foos: !include foo.yaml
//
// let foo.yaml contain the following contents
// someother: key
// bar: !include bar.yaml
//
// contents of bar.yaml
// baz: baz
//
// On execution of GetIncludeTagFiles(rootfile), It is expected to return
// rootfileparent/foo.yaml, fooparent/bar.yaml
func GetIncludeTagFiles(node *yaml.Node, baseDirectory string) ([]string, error) {
	var filenames []string
	_, err := resolveTags(map[string]string{baseDirectoryKey: baseDirectory}, node, &filenames)
	return filenames, err
}
