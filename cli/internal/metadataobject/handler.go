package metadataobject

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"

	"github.com/hasura/graphql-engine/cli/v2"

	gyaml "github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/mitchellh/mapstructure"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/afero"
	"gopkg.in/yaml.v2"
)

// Handler will be responsible for interaction between a hasura instance and Objects
type Handler struct {
	objects       Objects
	v1MetadataOps hasura.CommonMetadataOperations
	v2MetadataOps hasura.V2CommonMetadataOperations

	logger *logrus.Logger
}

func NewHandler(objects Objects, v1MetadataOps hasura.CommonMetadataOperations, v2MetadataOps hasura.V2CommonMetadataOperations, logger *logrus.Logger) *Handler {
	return &Handler{objects, v1MetadataOps, v2MetadataOps, logger}
}

func NewHandlerFromEC(ec *cli.ExecutionContext) *Handler {
	metadataObjects := GetMetadataObjectsWithDir(ec)
	return NewHandler(metadataObjects, cli.GetCommonMetadataOps(ec), ec.APIClient.V1Metadata, ec.Logger)
}

func (h *Handler) SetMetadataObjects(objects Objects) {
	h.objects = objects
}

// WriteMetadata writes the files in the metadata folder
func (h *Handler) WriteMetadata(files map[string][]byte) error {
	for name, content := range files {
		fs := afero.NewOsFs()
		if err := fs.MkdirAll(filepath.Dir(name), os.ModePerm); err != nil {
			return err
		}
		err := afero.WriteFile(fs, name, content, 0644)
		if err != nil {
			return errors.Wrapf(err, "creating metadata file %s failed", name)
		}
	}
	return nil
}

func (h *Handler) ExportMetadata() (map[string][]byte, error) {
	metadataFiles := make(map[string][]byte)
	var resp io.Reader
	var err error
	resp, err = h.v1MetadataOps.ExportMetadata()
	if err != nil {
		return nil, err
	}
	var c yaml.MapSlice
	err = yaml.NewDecoder(resp).Decode(&c)
	if err != nil {
		return nil, err
	}
	for _, object := range h.objects {
		files, err := object.Export(c)
		if err != nil {
			return nil, errors.Wrap(err, fmt.Sprintf("cannot export %s from metadata", object.Name()))
		}
		for fileName, content := range files {
			metadataFiles[fileName] = content
		}
	}
	return metadataFiles, nil
}

func (h *Handler) ResetMetadata() error {
	var err error
	_, err = h.v1MetadataOps.ClearMetadata()
	return err
}

// ReloadMetadata - Reload Hasura GraphQL Engine metadata on the database
func (h *Handler) ReloadMetadata() (io.Reader, error) {
	var err error
	r, err := h.v1MetadataOps.ReloadMetadata()
	return r, err
}

func (h *Handler) BuildMetadata() (yaml.MapSlice, error) {
	var tmpMeta yaml.MapSlice
	for _, object := range h.objects {
		err := object.Build(&tmpMeta)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				h.logger.Debugf("metadata file for %s was not found, assuming an empty file", object.Name())
				continue
			}
			return tmpMeta, errors.Wrap(err, fmt.Sprintf("cannot build %s from project", object.Name()))
		}
	}
	return tmpMeta, nil
}

func (h *Handler) MakeJSONMetadata() ([]byte, error) {
	tmpMeta, err := h.BuildMetadata()
	if err != nil {
		return nil, err
	}
	yByt, err := yaml.Marshal(tmpMeta)
	if err != nil {
		return nil, err
	}
	jbyt, err := gyaml.YAMLToJSON(yByt)
	if err != nil {
		return nil, err
	}
	return jbyt, nil
}

func (h *Handler) V1ApplyMetadata() (io.Reader, error) {
	jbyt, err := h.MakeJSONMetadata()
	if err != nil {
		return nil, err
	}
	r, err := h.v1MetadataOps.ReplaceMetadata(bytes.NewReader(jbyt))
	if err != nil {
		return nil, err
	}
	return r, nil
}

func (h *Handler) V2ApplyMetadata() (*hasura.V2ReplaceMetadataResponse, error) {
	jbyt, err := h.MakeJSONMetadata()
	if err != nil {
		return nil, err
	}
	var metadata interface{}
	if err := json.Unmarshal(jbyt, &metadata); err != nil {
		return nil, err
	}
	r, err := h.v2MetadataOps.V2ReplaceMetadata(hasura.V2ReplaceMetadataArgs{
		AllowInconsistentMetadata: true,
		Metadata:                  metadata,
	})
	if err != nil {
		return nil, err
	}
	return r, nil
}

func (h *Handler) GetInconsistentMetadata() (bool, []InconsistentMetadataObject, error) {
	inconsistentMetadata, err := h.v1MetadataOps.GetInconsistentMetadata()
	if err != nil {
		return true, nil, err
	}
	var objects []InconsistentMetadataObject
	err = mapstructure.Decode(inconsistentMetadata.InconsistentObjects, &objects)
	return inconsistentMetadata.IsConsistent, objects, nil
}

func (h *Handler) DropInconsistentMetadata() error {
	var err error
	_, err = h.v1MetadataOps.DropInconsistentMetadata()
	return err
}

type InconsistentMetadataObject struct {
	Definition interface{} `json:"definition" mapstructure:"definition"`
	Reason     interface{} `json:"reason" mapstructure:"reason"`
	Type       interface{} `json:"type" mapstructure:"type"`
}

/*
[
    {
        "definition": {
            "using": {
                "foreign_key_constraint_on": {
                    "column": "author_id",
                    "table": "article"
                }
            },
            "name": "articles",
            "comment": null,
            "table": "author"
        },
        "reason": "table \"article\" does not exist",
        "type": "array_relation"
    },
    {
        "definition": {
            "using": {
                "foreign_key_constraint_on": "author_id"
            },
            "name": "author",
            "comment": null,
            "table": "article"
        },
        "reason": "table \"article\" does not exist",
        "type": "object_relation"
    },
    {
        "definition": "article",
        "reason": "no such table/view exists in postgres : \"article\"",
        "type": "table"
    }
]
*/

func (obj InconsistentMetadataObject) GetType() string {
	if v, ok := obj.Type.(string); ok {
		return v
	}
	return "N/A"
}

func (obj InconsistentMetadataObject) GetName() string {
	var m map[string]interface{}
	if err := mapstructure.Decode(obj.Definition, &m); err == nil {
		if v, ok := m["name"]; ok {
			return fmt.Sprintf("%v", v)
		}
	}
	return "N/A"
}

func (obj InconsistentMetadataObject) GetDescription() string {
	b, err := json.Marshal(obj.Definition)
	if err == nil {
		return fmt.Sprintf("%.50s...", string(b))
	}
	return "N/A"
}

func (obj InconsistentMetadataObject) GetReason() string {
	if v, ok := obj.Reason.(string); ok {
		return v
	}
	b, err := json.Marshal(obj.Reason)
	if err == nil {
		return fmt.Sprintf("%.80s...", string(b))
	}
	return "N/A"
}
