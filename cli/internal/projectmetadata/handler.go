package projectmetadata

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"

	internalerrors "github.com/hasura/graphql-engine/cli/v2/internal/errors"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"

	"github.com/hasura/graphql-engine/cli/v2"

	"github.com/hasura/graphql-engine/cli/v2/internal/hasura"
	"github.com/mitchellh/mapstructure"
	"github.com/sirupsen/logrus"
	"github.com/spf13/afero"
	"gopkg.in/yaml.v3"
)

// Handler will be responsible for interaction between a hasura instance and Objects
type Handler struct {
	objects       metadataobject.Objects
	v1MetadataOps hasura.CommonMetadataOperations
	v2MetadataOps hasura.V2CommonMetadataOperations

	logger *logrus.Logger
}

func NewHandler(objects metadataobject.Objects, v1MetadataOps hasura.CommonMetadataOperations, v2MetadataOps hasura.V2CommonMetadataOperations, logger *logrus.Logger) *Handler {
	return &Handler{objects, v1MetadataOps, v2MetadataOps, logger}
}

func NewHandlerFromEC(ec *cli.ExecutionContext) *Handler {
	metadataObjects := GetMetadataObjectsWithDir(ec)
	return NewHandler(metadataObjects, cli.GetCommonMetadataOps(ec), ec.APIClient.V1Metadata, ec.Logger)
}

func (h *Handler) SetMetadataObjects(objects metadataobject.Objects) {
	h.objects = objects
}

// WriteMetadata writes the files in the metadata folder
func (h *Handler) WriteMetadata(files map[string][]byte) error {
	var op internalerrors.Op = "projectmetadata.Handler.WriteMetadata"
	for name, content := range files {
		fs := afero.NewOsFs()
		if err := fs.MkdirAll(filepath.Dir(name), os.ModePerm); err != nil {
			return internalerrors.E(op, err)
		}
		err := afero.WriteFile(fs, name, content, 0644)
		if err != nil {
			return internalerrors.E(op, fmt.Errorf("creating metadata file %s failed: %w", name, err))
		}
	}
	return nil
}

func (h *Handler) ExportMetadata() (map[string][]byte, error) {
	var op internalerrors.Op = "projectmetadata.Handler.ExportMetadata"
	metadataFiles := make(map[string][]byte)
	var resp io.Reader
	var err error
	resp, err = h.v1MetadataOps.ExportMetadata()
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	jsonmdbs, err := ioutil.ReadAll(resp)
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	// We don't want to strongly type metadata here, but we cannot use a catch-all kind of datastructures like
	// map[string]interface{} here because it'll mess up the ordering.
	// So we directly translate JSON to YAML, to preserve it.
	yamlmdbs, err := metadatautil.JSONToYAML(jsonmdbs)
	if err != nil {
		return nil, internalerrors.E(op, err)
	}

	var c map[string]yaml.Node
	err = yaml.NewDecoder(bytes.NewReader(yamlmdbs)).Decode(&c)
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	for _, object := range h.objects {
		files, err := object.Export(c)
		if err != nil {
			return nil, internalerrors.E(op, fmt.Errorf("cannot export %s from metadata: %w", object.Key(), err))
		}
		for fileName, content := range files {
			metadataFiles[fileName] = content
		}
	}
	return metadataFiles, nil
}

func (h *Handler) ResetMetadata() error {
	var op internalerrors.Op = "projectmetadata.Handler.ResetMetadata"
	var err error
	_, err = h.v1MetadataOps.ClearMetadata()
	if err != nil {
		return internalerrors.E(op, err)
	}
	return nil
}

// ReloadMetadata - Reload Hasura GraphQL Engine metadata on the database
func (h *Handler) ReloadMetadata() (io.Reader, error) {
	var op internalerrors.Op = "projectmetadata.Handler.ReloadMetadata"
	var err error
	r, err := h.v1MetadataOps.ReloadMetadata()
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	return r, nil
}

func (h *Handler) buildMetadataMap() (map[string]interface{}, error) {
	var op internalerrors.Op = "projectmetadata.Handler.buildMetadataMap"
	var metadata = map[string]interface{}{}
	for _, object := range h.objects {
		objectMetadata, err := object.Build()
		if err != nil {
			if errors.Is(err, metadataobject.ErrMetadataFileNotFound) {
				h.logger.Debugf("metadata file for %s was not found, assuming an empty file", object.Key())
				continue
			}
			return nil, internalerrors.E(op, fmt.Errorf("cannot build %s from project: %w", object.Key(), err))
		}
		for k, v := range objectMetadata {
			metadata[k] = v
		}
	}
	return metadata, nil
}

// buildMetadata is a private function because we don't intend consumers of this package
// to use the returned result (metadataobject.Metadata) directly because they may assume that they can use
// json.Marshal to get JSON representation of the built metadata. But this assumption will not hold true because
// the underlying types might have instances of yaml.Node which is not friendly with a json.Marshal and can produce
// unexpected results. Rather to get a JSON / YAML form of built metadata make use of Handler.BuildYAMLMetadata and
// Handler.BuildJSONMetadata helper functions
func (h *Handler) buildMetadata() (*Metadata, error) {
	var op internalerrors.Op = "projectmetadata.Handler.buildMetadata"
	metadataMap, err := h.buildMetadataMap()
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	return GenMetadataFromMap(metadataMap)
}

func (h *Handler) BuildYAMLMetadata() ([]byte, error) {
	var op internalerrors.Op = "projectmetadata.Handler.BuildYAMLMetadata"
	metadata, err := h.buildMetadata()
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	return metadata.YAML()
}

func (h *Handler) BuildJSONMetadata() ([]byte, error) {
	var op internalerrors.Op = "projectmetadata.Handler.BuildJSONMetadata"
	metadata, err := h.buildMetadata()
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	return metadata.JSON()
}

func (h *Handler) V1ApplyMetadata() (io.Reader, error) {
	var op internalerrors.Op = "projectmetadata.Handler.V1ApplyMetadata"
	jbyt, err := h.BuildJSONMetadata()
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	r, err := h.v1MetadataOps.ReplaceMetadata(bytes.NewReader(jbyt))
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	return r, nil
}

func (h *Handler) V2ApplyMetadata(disallowInconsistentMetadata bool) (*hasura.V2ReplaceMetadataResponse, error) {
	var op internalerrors.Op = "projectmetadata.Handler.V2ApplyMetadata"
	jbyt, err := h.BuildJSONMetadata()
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	var metadata interface{}
	if err := json.Unmarshal(jbyt, &metadata); err != nil {
		return nil, internalerrors.E(op, internalerrors.KindBadInput, err)
	}
	r, err := h.v2MetadataOps.V2ReplaceMetadata(hasura.V2ReplaceMetadataArgs{
		AllowInconsistentMetadata: !disallowInconsistentMetadata,
		Metadata:                  metadata,
	})
	if err != nil {
		return nil, internalerrors.E(op, err)
	}
	return r, nil
}

func (h *Handler) GetInconsistentMetadata() (bool, []InconsistentMetadataObject, error) {
	var op internalerrors.Op = "projectmetadata.Handler.GetInconsistentMetadata"
	inconsistentMetadata, err := h.v1MetadataOps.GetInconsistentMetadata()
	if err != nil {
		return true, nil, internalerrors.E(op, err)
	}
	var objects []InconsistentMetadataObject
	err = mapstructure.Decode(inconsistentMetadata.InconsistentObjects, &objects)
	if err != nil {
		return inconsistentMetadata.IsConsistent, objects, internalerrors.E(op, err)
	}
	return inconsistentMetadata.IsConsistent, objects, nil
}

func (h *Handler) DropInconsistentMetadata() error {
	var op internalerrors.Op = "projectmetadata.Handler.DropInconsistentMetadata"
	var err error
	_, err = h.v1MetadataOps.DropInconsistentMetadata()
	if err != nil {
		return internalerrors.E(op, err)
	}
	return nil
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

// Metadata does not strictly mirror the actual structure of server metadata
// this is evident in the struct below, because V3 metadata does not contain "tables" / "functions" key
//
// this is rather a utility / helper struct which allow us to unmarshal / marshal
// metadata bytes in a specific order.
type Metadata struct {
	Version          interface{} `yaml:"version" mapstructure:"version"`
	Sources          interface{} `yaml:"sources,omitempty" mapstructure:"sources,omitempty"`
	Tables           interface{} `yaml:"tables,omitempty" mapstructure:"tables,omitempty"`
	Functions        interface{} `yaml:"functions,omitempty" mapstructure:"functions,omitempty"`
	Actions          interface{} `yaml:"actions,omitempty" mapstructure:"actions,omitempty"`
	CustomTypes      interface{} `yaml:"custom_types,omitempty" mapstructure:"custom_types,omitempty"`
	RemoteSchemas    interface{} `yaml:"remote_schemas,omitempty" mapstructure:"remote_schemas,omitempty"`
	QueryCollections interface{} `yaml:"query_collections,omitempty" mapstructure:"query_collections,omitempty"`
	AllowList        interface{} `yaml:"allowlist,omitempty" mapstructure:"allowlist,omitempty"`
	CronTriggers     interface{} `yaml:"cron_triggers,omitempty" mapstructure:"cron_triggers,omitempty"`
	Network          interface{} `yaml:"network,omitempty" mapstructure:"network,omitempty"`
	APILimits        interface{} `yaml:"api_limits,omitempty" mapstructure:"api_limits,omitempty"`
	RestEndpoints    interface{} `yaml:"rest_endpoints,omitempty" mapstructure:"rest_endpoints,omitempty"`
	InheritedRoles   interface{} `yaml:"inherited_roles,omitempty" mapstructure:"inherited_roles,omitempty"`
	Opentelemetry    interface{} `yaml:"opentelemetry,omitempty" mapstructure:"opentelemetry,omitempty"`
	BackendConfig    interface{} `yaml:"backend_configs,omitempty" mapstructure:"backend_configs,omitempty"`

	// HGE Pro
	GraphQLSchemaIntrospection interface{} `yaml:"graphql_schema_introspection,omitempty" mapstructure:"graphql_schema_introspection,omitempty"`
	MetricsConfig              interface{} `yaml:"metrics_config,omitempty" mapstructure:"metrics_config,omitempty"`

	// note: update metadatautil/json.metadata to reflect changes made here
}

// JSON is a helper function which returns JSON representation of Metadata
// This exists because we cannot directly do a json.Marshal on Metadata because
// the underlying type of struct fields might be yaml.Node
// which might give unintended result
func (m Metadata) JSON() ([]byte, error) {
	var op internalerrors.Op = "projectmetadata.Metadata.JSON"
	yamlbs, err := m.YAML()
	if err != nil {
		return nil, internalerrors.E(op, err)
	}

	return metadatautil.YAMLToJSON(yamlbs)
}

func (m Metadata) YAML() ([]byte, error) {
	var op internalerrors.Op = "projectmetadata.Metadata.YAML"
	var buf bytes.Buffer
	if err := metadataobject.GetEncoder(&buf).Encode(m); err != nil {
		return nil, internalerrors.E(op, err)
	}
	return buf.Bytes(), nil
}

func GenMetadataFromMap(metadata map[string]interface{}) (*Metadata, error) {
	var op internalerrors.Op = "projectmetadata.GenMetadataFromMap"
	var m = new(Metadata)
	if err := mapstructure.Decode(metadata, m); err != nil {
		return nil, internalerrors.E(op, err)
	}
	return m, nil
}
