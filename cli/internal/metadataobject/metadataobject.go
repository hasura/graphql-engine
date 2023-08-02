package metadataobject

import (
	"bytes"
	stderrors "errors"
	"fmt"
	"io"
	"io/fs"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/gonvenience/ytbx"
	"github.com/hasura/graphql-engine/cli/v2/internal/diff"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"gopkg.in/yaml.v3"
)

const (
	VersionKey                    string = "version"
	SourcesKey                    string = "sources"
	TablesKey                     string = "tables"
	FunctionsKey                  string = "functions"
	ActionsKey                    string = "actions"
	CustomTypesKey                string = "custom_types"
	RemoteSchemasKey              string = "remote_schemas"
	QueryCollectionsKey           string = "query_collections"
	AllowListKey                  string = "allowlist"
	CronTriggersKey               string = "cron_triggers"
	APILimitsKey                  string = "api_limits"
	RestEndpointsKey              string = "rest_endpoints"
	InheritedRolesKey             string = "inherited_roles"
	GraphQLSchemaIntrospectionKey string = "graphql_schema_introspection"
	NetworkKey                    string = "network"
	MetricsConfigKey              string = "metrics_config"
	OpentelemetryKey              string = "opentelemetry"
	BackendConfigsKey             string = "backend_configs"
)

type Objects []Object

type WriteDiffOpts struct {
	To           Object
	W            io.Writer
	DisableColor bool
}

// GetEncoder is the YAML encoder which is expected to be used in all implementations
// of Object. This helps bring uniformity in the format of generated YAML
func GetEncoder(destination io.Writer) *yaml.Encoder {
	encoder := yaml.NewEncoder(destination)
	encoder.SetIndent(2)
	return encoder
}

type Object interface {
	// Build will be responsible for reading the metadata object from metadata files in project directory
	// It should return nil if it was not able to find the matching metadata file
	// Build returns a map of the objects, and it's corresponding unmarshalled content
	// For example:
	// an implementation for handling the "remote_schemas" object might return a map like
	// "remote_schemas": []yaml.Node
	// since there is a chance that this function can return a yaml.Node. The return value is not expected to
	// directly be unmarshalled to JSON using json.Marshal
	Build() (map[string]interface{}, error)
	// Export is responsible for writing the yaml Node(s) for the metadata object
	// to project directory
	// Export expects a map[string]yaml.Node specifically rather than a builtin data structure like
	// map[string]interface{} because it does not guarantee the order in which contents will be unmarshalled.
	// eg: say we received the following JSON metadata from the server
	// {
	//		"foo": [
	//			"x": 1,
	//			"a": 2,
	//		],
	// }
	// we are interested in preserving the order of keys when transforming this to YAML. Something like the following
	// foo:
	// 	x: 1
	// 	a: 2
	// This ordering is not guaranteed if we are using map[string]interface{} to unmarshal the JSON. This might look
	// something like the following
	// foo:
	// 	a: 2
	// 	x: 1
	// This not bug, since JSON spec doesn't guarantee the ordering anyway.
	// We are interested in writing or transforming the JSON object received from the server in
	// the same order to YAML files. This coupled with our requirement of NOT strongly typing metadata on CLI requires
	// using yaml.Node to preserve the ordering.
	Export(metadata map[string]yaml.Node) (map[string][]byte, error)
	CreateFiles() error
	// GetFiles will return an array of file paths which make up the metadata object.
	// For example the "sources" metadata key is made up of files like
	// databases.yaml. which then will have !include tags which will branch to include
	// files like databases/<source-name>/tables/tables.yaml
	// this function is expected to return the list of all these files which make up the metadata object
	GetFiles() ([]string, error)
	// WriteDiff should be implemented such that it should write the difference
	// between the current object and passed in object on the provided writer
	WriteDiff(WriteDiffOpts) error
	Key() string
	Filename() string
	// BaseDirectory will return the parent directory of `Filename()`
	BaseDirectory() string
}

var ErrMetadataFileNotFound = fmt.Errorf("metadata file not found")

func ReadMetadataFile(filename string) ([]byte, error) {
	var op errors.Op = "metadataobject.ReadMetadataFile"
	bytes, err := ioutil.ReadFile(filename)
	if err != nil && stderrors.Is(err, fs.ErrNotExist) {
		return nil, errors.E(op, ErrMetadataFileNotFound)
	}
	if err != nil {
		return nil, errors.E(op, err)
	}
	return bytes, nil
}

type ErrParsingMetadataObject interface {
	// ObjectName corresponds to metadata object in JSON format
	// eg: source, api_limits etc
	ObjectName() string
	// Filename corresponding to metadata object
	// eg: databases.yaml, actions.yaml
	Filename() string
	// ErrorContext will contain any additional information regarding error
	ErrorContext() string

	// Unwrap will make sure the error returned is unwrappable
	// https://blog.golang.org/go1.13-errors#TOC_3.1.
	Unwrap() error
	error
}

func NewErrParsingMetadataObject(o Object, err error, context ...string) ErrParsingMetadataObject {
	return &errParsingMetadataObjectFile{o.Key(), o.Filename(), context, err}
}

type errParsingMetadataObjectFile struct {
	objectName string
	fileName   string
	context    []string
	err        error
}

func (e *errParsingMetadataObjectFile) ObjectName() string {
	return e.objectName
}

func (e *errParsingMetadataObjectFile) Filename() string {
	return e.fileName
}

func (e *errParsingMetadataObjectFile) ErrorContext() string {
	if len(e.context) != 0 {
		return fmt.Sprintf("context: %v\n", strings.Join(e.context, "\n"))
	}
	return ""
}

func (e *errParsingMetadataObjectFile) Error() string {
	return strings.Trim(fmt.Sprintf(
		"\nerror parsing metadata \nobject: %v\nfile: %v\nerror: %v\n%v",
		e.ObjectName(),
		e.Filename(),
		e.err.Error(),
		e.ErrorContext(),
	), "\n")
}

func (e *errParsingMetadataObjectFile) Unwrap() error { return e.err }

// DefaultGetFiles is a default implementation for Object.GetFiles
func DefaultGetFiles(yamlFile string) ([]string, error) {
	var op errors.Op = "metadataobject.DefaultGetFiles"
	rootFileContents, err := ioutil.ReadFile(yamlFile)
	if err != nil {
		if stderrors.Is(err, os.ErrNotExist) {
			return nil, nil
		}
		return nil, errors.E(op, err)
	}
	var rootIsMapArray []map[string]yaml.Node
	files := []string{yamlFile}
	parentDir := filepath.Dir(yamlFile)
	err = yaml.Unmarshal(rootFileContents, &rootIsMapArray)
	if err == nil {
		for _, item := range rootIsMapArray {
			for _, node := range item {
				nodeFiles, err := metadatautil.GetIncludeTagFiles(&node, parentDir)
				if err != nil {
					return nil, errors.E(op, err)
				}
				files = append(files, nodeFiles...)
			}
		}
	} else {
		var rootIsMap map[string]yaml.Node
		err = yaml.Unmarshal(rootFileContents, &rootIsMap)
		if err == nil {
			for _, node := range rootIsMap {
				nodeFiles, err := metadatautil.GetIncludeTagFiles(&node, parentDir)
				if err != nil {
					return nil, errors.E(op, err)
				}
				files = append(files, nodeFiles...)
			}
		} else {
			return nil, errors.E(op, fmt.Errorf("finding child files in failed: %w", err))
		}
	}

	return files, nil
}

type DefaultWriteDiffOpts struct {
	From Object
	WriteDiffOpts
	ExcludeFilesPatterns []string
}

// DefaultWriteDiff is the default implementation for Object.WriteDiff
func DefaultWriteDiff(opts DefaultWriteDiffOpts) error {
	var op errors.Op = "metadataobject.DefaultWriteDiff"
	var err error
	var fromFiles, toFiles []string
	// we want to diff between the current object and to
	fromFiles, err = opts.From.GetFiles()
	if err != nil {
		return errors.E(op, err)
	}
	toFiles, err = opts.To.GetFiles()
	if err != nil {
		return errors.E(op, err)
	}
	fromFiles, err = cleanExcludedPatterns(fromFiles, opts.ExcludeFilesPatterns)
	if err != nil {
		return errors.E(op, err)
	}
	toFiles, err = cleanExcludedPatterns(toFiles, opts.ExcludeFilesPatterns)
	if err != nil {
		return errors.E(op, err)
	}
	fromFilesMap, toFilesMap := map[string]diffFile{}, map[string]diffFile{}
	for _, file := range fromFiles {
		relativePath, err := filepath.Rel(opts.From.BaseDirectory(), file)
		if err != nil {
			return errors.E(op, err)
		}
		fromFilesMap[relativePath] = diffFile{false, file, relativePath}
	}
	for _, file := range toFiles {
		relativePath, err := filepath.Rel(opts.To.BaseDirectory(), file)
		if err != nil {
			return errors.E(op, err)
		}

		toFilesMap[relativePath] = diffFile{false, file, relativePath}
	}
	for _, file := range fromFiles {
		fromFileRelativeFilepath, err := filepath.Rel(opts.From.BaseDirectory(), file)
		if err != nil {
			return errors.E(op, err)
		}
		fromFileDetails, foundFromRelativeFilepath := fromFilesMap[fromFileRelativeFilepath]
		toFileDetails, foundToFileRelativeFilepath := toFilesMap[fromFileRelativeFilepath]
		if foundFromRelativeFilepath && foundToFileRelativeFilepath {
			if diff.IsYAMLFile(fromFileRelativeFilepath) {
				fromYAML, err := ytbx.LoadFile(fromFileDetails.fullFilePath)
				if err != nil {
					return errors.E(op, err)
				}
				toYAML, err := ytbx.LoadFile(toFileDetails.fullFilePath)
				if err != nil {
					return errors.E(op, err)
				}
				toFileDetails.processed = true
				toFilesMap[fromFileRelativeFilepath] = toFileDetails
				_, err = diff.YamlDiff(fromYAML, toYAML, opts.W, fromFileRelativeFilepath)
				if err != nil {
					return errors.E(op, err)
				}
			} else {
				fromFileBytes, err := ioutil.ReadFile(fromFileDetails.fullFilePath)
				if err != nil {
					return errors.E(op, err)
				}
				toFileBytes, err := ioutil.ReadFile(toFileDetails.fullFilePath)
				if err != nil {
					return errors.E(op, err)
				}
				toFileDetails.processed = true
				toFilesMap[fromFileRelativeFilepath] = toFileDetails
				var buf bytes.Buffer
				diffCount, err := diff.MyersDiff(string(fromFileBytes), string(toFileBytes), fromFileRelativeFilepath, fromFileRelativeFilepath, &buf, opts.DisableColor)
				if err != nil {
					return errors.E(op, err)
				}
				if diffCount > 0 {
					fmt.Fprintln(opts.W, fromFileRelativeFilepath)
					_, err := io.Copy(opts.W, &buf)
					if err != nil {
						return errors.E(op, err)
					}
					fmt.Fprintln(opts.W)
				}
			}
		} else {
			if err := diffWithEmptyFile(fromFileDetails, diffDirectionFrom, opts.W, opts.DisableColor); err != nil {
				return errors.E(op, err)
			}
		}
		fromFileDetails.processed = true
		fromFilesMap[fromFileRelativeFilepath] = fromFileDetails
	}
	for _, file := range toFiles {
		toFileRelativeFilepath, err := filepath.Rel(opts.To.BaseDirectory(), file)
		if err != nil {
			return errors.E(op, err)
		}
		if !toFilesMap[toFileRelativeFilepath].processed {
			if err := diffWithEmptyFile(toFilesMap[toFileRelativeFilepath], diffDirectionTo, opts.W, opts.DisableColor); err != nil {
				return errors.E(op, err)
			}
		}
	}
	return nil
}

// createEmptyYamlFileAccordingToContent helps to generate a ytbx.InputFile
// if file is a document which contains YAML Sequence nodes it'll return a YAML
// file of an empty sequence ([]), otherwise it returns an MappingNode
func createEmptyYamlFileAccordingToContent(file ytbx.InputFile) ytbx.InputFile {
	//Empty yaml data
	var documentNode *yaml.Node
	for _, node := range file.Documents {
		documentNode = node
		break
	}
	var contentNode *yaml.Node
	for _, n := range documentNode.Content {
		contentNode = n
		break
	}
	var content interface{}
	if contentNode != nil && contentNode.Kind == yaml.SequenceNode {
		content = []map[string]string{}
	} else {
		content = map[string]string{}
	}
	emptyBytes, _ := yaml.Marshal(content)
	emptyNode, _ := ytbx.LoadYAMLDocuments(emptyBytes)
	emptyInputFile := ytbx.InputFile{Documents: emptyNode}
	return emptyInputFile
}

func cleanExcludedPatterns(files []string, patterns []string) ([]string, error) {
	var cleaned []string
	for _, file := range files {
		matched := false
		for _, pattern := range patterns {
			matched = strings.Contains(file, pattern)
			if matched {
				break
			}
		}
		if !matched {
			cleaned = append(cleaned, file)
		}
	}
	return cleaned, nil
}

type diffDirection int

const (
	diffDirectionTo   diffDirection = 1
	diffDirectionFrom diffDirection = 2
)

type diffFile struct {
	processed    bool
	fullFilePath string
	relativePath string
}

func diffWithEmptyFile(file diffFile, direction diffDirection, w io.Writer, disableColor bool) error {
	var op errors.Op = "metadataobject.diffWithEmptyFile"
	if diff.IsYAMLFile(file.fullFilePath) {
		yamlFile, err := ytbx.LoadFile(file.fullFilePath)
		if err != nil {
			return errors.E(op, err)
		}
		if direction == diffDirectionTo {
			_, err = diff.YamlDiff(createEmptyYamlFileAccordingToContent(yamlFile), yamlFile, w, file.relativePath)
			if err != nil {
				return errors.E(op, err)
			}
		} else {
			_, err = diff.YamlDiff(yamlFile, createEmptyYamlFileAccordingToContent(yamlFile), w, file.relativePath)
			if err != nil {
				return errors.E(op, err)
			}
		}
	} else {
		fileBytes, err := ioutil.ReadFile(file.fullFilePath)
		if err != nil {
			return errors.E(op, err)
		}

		var buf bytes.Buffer
		var diffCount int
		if direction == diffDirectionTo {
			diffCount, err = diff.MyersDiff("", string(fileBytes), file.relativePath, file.relativePath, &buf, disableColor)
		} else {
			diffCount, err = diff.MyersDiff(string(fileBytes), "", file.relativePath, file.relativePath, &buf, disableColor)
		}
		if err != nil {
			return errors.E(op, err)
		}
		if diffCount > 0 {
			fmt.Fprintln(w, file.relativePath)
			_, err := io.Copy(w, &buf)
			if err != nil {
				return errors.E(op, err)
			}
			fmt.Fprintln(w)
		}
	}
	return nil
}

type DefaultObjectType int

const (
	DefaultObjectTypeSequence DefaultObjectType = iota
	DefaultObjectTypeMapping
)

// DefaultExport is an implementation for Object.Export
// metadata objects which doesn't require additional customisations can make use of this implementation
func DefaultExport(object Object, metadata map[string]yaml.Node, errorFunc func(error, ...string) ErrParsingMetadataObject, objectType DefaultObjectType) (map[string][]byte, ErrParsingMetadataObject) {
	var value interface{}
	if v, ok := metadata[object.Key()]; ok {
		value = v
	} else {
		switch objectType {
		case DefaultObjectTypeSequence:
			value = yaml.Node{Kind: yaml.SequenceNode}
		case DefaultObjectTypeMapping:
			value = yaml.Node{Kind: yaml.MappingNode}
		default:
			value = nil
		}
	}

	var buf bytes.Buffer
	err := GetEncoder(&buf).Encode(value)
	if err != nil {
		return nil, errorFunc(err)
	}
	return map[string][]byte{
		filepath.ToSlash(filepath.Join(object.BaseDirectory(), object.Filename())): buf.Bytes(),
	}, nil
}
