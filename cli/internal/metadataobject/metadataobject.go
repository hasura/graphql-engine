package metadataobject

import (
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"

	"github.com/gonvenience/ytbx"
	"github.com/hasura/graphql-engine/cli/v2/internal/diff"

	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	v3yaml "gopkg.in/yaml.v3"

	"gopkg.in/yaml.v2"
)

type Objects []Object

type WriteDiffOpts struct {
	To           Object
	W            io.Writer
	DisableColor bool
}
type Object interface {
	Build(metadata *yaml.MapSlice) ErrParsingMetadataObject
	Export(metadata yaml.MapSlice) (map[string][]byte, ErrParsingMetadataObject)
	CreateFiles() error
	// GetFiles will return an array of filepaths which make up the metadata object.
	// For example the "sources" metadata key is made up of files like
	// databases.yaml. which then will have !include tags which will branch to include
	// files like databases/<source-name>/tables/tables.yaml
	// this function is expected to return the list of all these files which make up the metadata object
	GetFiles() ([]string, ErrParsingMetadataObject)
	// WriteDiff should be implemented such that it should write the difference
	// between the current object and passed in object on the provided writer
	WriteDiff(WriteDiffOpts) ErrParsingMetadataObject
	Key() string
	Filename() string
	// BaseDirectory will return the parent directory of `Filename()`
	BaseDirectory() string
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
	rootFileContents, err := ioutil.ReadFile(yamlFile)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return nil, nil
		}
		return nil, err
	}
	var rootIsMapArray []map[string]v3yaml.Node
	files := []string{yamlFile}
	parentDir := filepath.Dir(yamlFile)
	err = v3yaml.Unmarshal(rootFileContents, &rootIsMapArray)
	if err == nil {
		for _, item := range rootIsMapArray {
			for _, node := range item {
				nodeFiles, err := metadatautil.GetIncludeTagFiles(&node, parentDir)
				if err != nil {
					return nil, err
				}
				files = append(files, nodeFiles...)
			}
		}
	} else {
		var rootIsMap map[string]v3yaml.Node
		err = v3yaml.Unmarshal(rootFileContents, &rootIsMap)
		if err == nil {
			for _, node := range rootIsMap {
				nodeFiles, err := metadatautil.GetIncludeTagFiles(&node, parentDir)
				if err != nil {
					return nil, err
				}
				files = append(files, nodeFiles...)
			}
		} else {
			return nil, fmt.Errorf("finding child files in failed: %w", err)
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
	var err error
	var fromFiles, toFiles []string
	// we want to diff between the current object and to
	fromFiles, err = opts.From.GetFiles()
	if err != nil {
		return err
	}
	toFiles, err = opts.To.GetFiles()
	if err != nil {
		return err
	}
	fromFiles, err = cleanExcludedPatterns(fromFiles, opts.ExcludeFilesPatterns)
	if err != nil {
		return err
	}
	toFiles, err = cleanExcludedPatterns(toFiles, opts.ExcludeFilesPatterns)
	if err != nil {
		return err
	}
	fromFilesMap, toFilesMap := map[string]diffFile{}, map[string]diffFile{}
	for _, file := range fromFiles {
		relativePath, err := filepath.Rel(opts.From.BaseDirectory(), file)
		if err != nil {
			return err
		}
		fromFilesMap[relativePath] = diffFile{false, file, relativePath}
	}
	for _, file := range toFiles {
		relativePath, err := filepath.Rel(opts.To.BaseDirectory(), file)
		if err != nil {
			return err
		}

		toFilesMap[relativePath] = diffFile{false, file, relativePath}
	}
	for _, file := range fromFiles {
		fromFileRelativeFilepath, err := filepath.Rel(opts.From.BaseDirectory(), file)
		if err != nil {
			return err
		}
		fromFileDetails, foundFromRelativeFilepath := fromFilesMap[fromFileRelativeFilepath]
		toFileDetails, foundToFileRelativeFilepath := toFilesMap[fromFileRelativeFilepath]
		if foundFromRelativeFilepath && foundToFileRelativeFilepath {
			if diff.IsYAMLFile(fromFileRelativeFilepath) {
				fromYAML, err := ytbx.LoadFile(fromFileDetails.fullFilePath)
				if err != nil {
					return err
				}
				toYAML, err := ytbx.LoadFile(toFileDetails.fullFilePath)
				if err != nil {
					return err
				}
				toFileDetails.processed = true
				toFilesMap[fromFileRelativeFilepath] = toFileDetails
				_, err = diff.YamlDiff(fromYAML, toYAML, opts.W, fromFileRelativeFilepath)
				if err != nil {
					return err
				}
			} else {
				fromFileBytes, err := ioutil.ReadFile(fromFileDetails.fullFilePath)
				if err != nil {
					return err
				}
				toFileBytes, err := ioutil.ReadFile(toFileDetails.fullFilePath)
				if err != nil {
					return err
				}
				toFileDetails.processed = true
				toFilesMap[fromFileRelativeFilepath] = toFileDetails
				var buf bytes.Buffer
				diffCount, err := diff.MyersDiff(string(fromFileBytes), string(toFileBytes), fromFileRelativeFilepath, fromFileRelativeFilepath, &buf, opts.DisableColor)
				if err != nil {
					return err
				}
				if diffCount > 0 {
					fmt.Fprintln(opts.W, fromFileRelativeFilepath)
					_, err := io.Copy(opts.W, &buf)
					if err != nil {
						return err
					}
					fmt.Fprintln(opts.W)
				}
			}
		} else {
			if err := diffWithEmptyFile(fromFileDetails, diffDirectionFrom, opts.W, opts.DisableColor); err != nil {
				return err
			}
		}
		fromFileDetails.processed = true
		fromFilesMap[fromFileRelativeFilepath] = fromFileDetails
	}
	for _, file := range toFiles {
		toFileRelativeFilepath, err := filepath.Rel(opts.To.BaseDirectory(), file)
		if err != nil {
			return err
		}
		if !toFilesMap[toFileRelativeFilepath].processed {
			if err := diffWithEmptyFile(toFilesMap[toFileRelativeFilepath], diffDirectionTo, opts.W, opts.DisableColor); err != nil {
				return err
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
	var documentNode *v3yaml.Node
	for _, node := range file.Documents {
		documentNode = node
		break
	}
	var contentNode *v3yaml.Node
	for _, n := range documentNode.Content {
		contentNode = n
		break
	}
	var content interface{}
	if contentNode != nil && contentNode.Kind == v3yaml.SequenceNode {
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
	if diff.IsYAMLFile(file.fullFilePath) {
		yamlFile, err := ytbx.LoadFile(file.fullFilePath)
		if err != nil {
			return err
		}
		if direction == diffDirectionTo {
			_, err = diff.YamlDiff(createEmptyYamlFileAccordingToContent(yamlFile), yamlFile, w, file.relativePath)
			if err != nil {
				return err
			}
		} else {
			_, err = diff.YamlDiff(yamlFile, createEmptyYamlFileAccordingToContent(yamlFile), w, file.relativePath)
			if err != nil {
				return err
			}
		}
	} else {
		fileBytes, err := ioutil.ReadFile(file.fullFilePath)
		if err != nil {
			return err
		}

		var buf bytes.Buffer
		var diffCount int
		if direction == diffDirectionTo {
			diffCount, err = diff.MyersDiff("", string(fileBytes), file.relativePath, file.relativePath, &buf, disableColor)
		} else {
			diffCount, err = diff.MyersDiff(string(fileBytes), "", file.relativePath, file.relativePath, &buf, disableColor)
		}
		if err != nil {
			return err
		}
		if diffCount > 0 {
			fmt.Fprintln(w, file.relativePath)
			_, err := io.Copy(w, &buf)
			if err != nil {
				return err
			}
			fmt.Fprintln(w)
		}
	}
	return nil
}
