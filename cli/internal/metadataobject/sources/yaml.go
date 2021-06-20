package sources

import (
	"fmt"
	"io/ioutil"
	"path/filepath"
	"strings"

	"gopkg.in/yaml.v3"
)

const includeTag = "!include"

type sourcesYamlDecoderOpts struct {
	// directory which is to be used as the parent directory to look for filenames
	// specified in !include tag
	IncludeTagBaseDirectory string
}
type sourcesYamlDecoder struct {
	destination interface{}
	opts        sourcesYamlDecoderOpts
}

func newSourcesYamlDecoder(opts sourcesYamlDecoderOpts, destination interface{}) *sourcesYamlDecoder {
	return &sourcesYamlDecoder{destination, opts}
}

func (s *sourcesYamlDecoder) UnmarshalYAML(value *yaml.Node) error {
	ctx := map[string]string{}
	ctx[includeTag] = s.opts.IncludeTagBaseDirectory

	resolved, err := resolveTags(ctx, value)
	if err != nil {
		return err
	}
	return resolved.Decode(s.destination)
}

type Fragment struct {
	ctx     map[string]string
	content *yaml.Node
}

func newFragment(ctx map[string]string) *Fragment {
	f := new(Fragment)
	f.ctx = ctx
	return f
}
func (f *Fragment) UnmarshalYAML(value *yaml.Node) error {
	var err error
	// process includes in fragments
	f.content, err = resolveTags(f.ctx, value)
	return err
}

func resolveTags(ctx map[string]string, node *yaml.Node) (*yaml.Node, error) {
	resolve := func(node *yaml.Node) (*yaml.Node, error) {
		if node.Kind != yaml.ScalarNode {
			return nil, fmt.Errorf("found %s on scalar node", includeTag)
		}
		baseDir, ok := ctx[includeTag]
		if !ok {
			return nil, fmt.Errorf("parser error: base directory for !include tag not specified")
		}
		fileLocation := filepath.Join(baseDir, node.Value)
		file, err := ioutil.ReadFile(fileLocation)
		if err != nil {
			return nil, fmt.Errorf("%s: %w", fileLocation, err)
		}
		newctx := map[string]string{}
		for k, v := range ctx {
			newctx[k] = v
		}
		newctx[includeTag] = filepath.Dir(filepath.Join(baseDir, node.Value))
		var f = newFragment(newctx)
		err = yaml.Unmarshal(file, f)
		if err != nil {
			return nil, fmt.Errorf("%s: %w", fileLocation, err)
		}
		return f.content, nil
	}
	switch node.Tag {
	case includeTag:
		return resolve(node)
	case "!!str":
		if strings.Contains(node.Value, includeTag) {
			node.Tag = includeTag
			parts := strings.Split(node.Value, " ")
			node.Value = strings.Trim(strings.Join(parts[1:], " "), "\"")
			return resolve(node)
		}
	}

	switch node.Kind {
	case yaml.DocumentNode, yaml.SequenceNode, yaml.MappingNode:
		var err error
		for idx := range node.Content {
			node.Content[idx], err = resolveTags(ctx, node.Content[idx])
			if err != nil {
				return nil, err
			}
		}
	}
	return node, nil
}

type IncludeTagVisitor struct {
	baseDir string
}
