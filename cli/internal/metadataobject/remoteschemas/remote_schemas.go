package remoteschemas

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/goccy/go-yaml"
	"github.com/hasura/graphql-engine/cli/v2"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadataobject"
	"github.com/hasura/graphql-engine/cli/v2/internal/metadatautil"
	"github.com/sirupsen/logrus"
	goyaml "gopkg.in/yaml.v2"
	v3yaml "gopkg.in/yaml.v3"
)

const (
	remoteSchemasDirectory                 string = "remote_schemas"
	remoteSchemasPermissionsDirectory      string = "permissions"
	remoteSchemaPermissionsFile            string = "permissions.yaml"
	remoteSchemaPermissionSchemasDirectory string = "schemas"
)

type RemoteSchema struct {
	Name       string      `yaml:"name"`
	Definition interface{} `yaml:"definition"`
	Comment    interface{} `yaml:"comment"`
	Permission interface{} `yaml:"permissions"`
}
type SchemaDefinition struct {
	Schema string `yaml:"schema"`
}

type RemoteSchemaConfig struct {
	MetadataDir string

	logger *logrus.Logger
}

func New(ec *cli.ExecutionContext, baseDir string) *RemoteSchemaConfig {
	return &RemoteSchemaConfig{
		MetadataDir: baseDir,
		logger:      ec.Logger,
	}
}

func (r *RemoteSchemaConfig) Validate() error {
	return nil
}

func (r *RemoteSchemaConfig) CreateFiles() error {
	v := make([]interface{}, 0)
	data, err := yaml.Marshal(v)
	if err != nil {
		return err
	}

	path := filepath.Join(r.MetadataDir, remoteSchemasDirectory, r.Filename())
	if err := os.MkdirAll(filepath.Dir(path), 0744); err != nil {
		return err
	}
	err = ioutil.WriteFile(path, data, 0644)
	if err != nil {
		return err
	}
	return nil
}

func (r *RemoteSchemaConfig) Build(metadata *goyaml.MapSlice) metadataobject.ErrParsingMetadataObject {

	newRemoteSchemaFile := filepath.Join(r.MetadataDir, remoteSchemasDirectory, r.Filename())
	if _, err := os.Stat(newRemoteSchemaFile); os.IsNotExist(err) {
		// if metadata/remotes_schemas/remote_schema.yaml is not present
		// fall back to old workflow where remote schemas used to stored in metadata/remote_schemas.yaml
		// if this file exists respect this and read metadata from here
		data, err := ioutil.ReadFile(filepath.Join(r.MetadataDir, r.Filename()))
		if err != nil {
			return r.error(err)
		}
		item := goyaml.MapItem{
			Key: "remote_schemas",
		}
		var obj []goyaml.MapSlice
		err = goyaml.Unmarshal(data, &obj)
		if err != nil {
			return r.error(err)
		}
		if len(obj) != 0 {
			item.Value = obj
			*metadata = append(*metadata, item)
		}
		return nil
	}

	data, err := ioutil.ReadFile(newRemoteSchemaFile)

	if err != nil {
		return r.error(err)
	}
	var remoteSchemas []*RemoteSchema
	if err := v3yaml.Unmarshal(data, &remoteSchemas); err != nil {
		return r.error(fmt.Errorf("parsing error: %w", err))
	}

	if len(remoteSchemas) == 0 {
		return nil
	}
	for idx, remoteSchema := range remoteSchemas {
		permissionPath := fmt.Sprintf("$[%d].permissions", idx)
		permissionsPath, err := yaml.PathString(permissionPath)
		if err != nil {
			return r.error(fmt.Errorf("parsing error: %w", err))
		}
		PermissionNode, err := permissionsPath.ReadNode(bytes.NewReader(data))
		if err == nil {
			tableNodeBytes, err := ioutil.ReadAll(PermissionNode)
			if err != nil {
				return r.error(err)
			}
			var permissions interface{}
			err = v3yaml.Unmarshal(tableNodeBytes, metadatautil.NewYamlDecoder(
				metadatautil.YamlDecoderOpts{
					IncludeTagBaseDirectory: filepath.Join(r.MetadataDir, remoteSchemasDirectory),
				},
				&permissions,
			))
			if err != nil {
				return r.error(err)
			}
			remoteSchema.Permission = permissions
		} else {
			r.logger.Debugf("building metadata: permission node not found for %s", remoteSchema.Name)
		}
	}

	item := goyaml.MapItem{
		Key:   "remote_schemas",
		Value: []yaml.MapSlice{},
	}
	item.Value = remoteSchemas
	*metadata = append(*metadata, item)
	return nil
}

func (r *RemoteSchemaConfig) Export(metadata goyaml.MapSlice) (map[string][]byte, metadataobject.ErrParsingMetadataObject) {

	oldRemoteSchemaFile := filepath.Join(r.MetadataDir, r.Filename())
	if _, err := os.Stat(oldRemoteSchemaFile); !os.IsNotExist(err) {
		err := os.Remove(oldRemoteSchemaFile)
		if err != nil {
			return nil, r.error(err, "error while removing old remote schema file in metadata directory")
		}
	}

	metadataBytes, err := goyaml.Marshal(metadata)
	if err != nil {
		return nil, r.error(err)
	}
	files := map[string][]byte{}
	var remoteSchemas []*RemoteSchema
	remoteSchemaPath, err := yaml.PathString("$.remote_schemas")
	if err != nil {
		return nil, r.error(err)
	}
	if err := remoteSchemaPath.Read(bytes.NewReader(metadataBytes), &remoteSchemas); err != nil {
		r.logger.Debug("reading remote schema from metadata", err)
	}

	for idx, remoteSchema := range remoteSchemas {

		permissions := make([]struct {
			Role       string           `yaml:"role"`
			Definition SchemaDefinition `yaml:"definition"`
		}, 0)

		permissionPath := fmt.Sprintf("$.remote_schemas[%d].permissions", idx)
		permissionsPath, err := yaml.PathString(permissionPath)
		if err != nil {
			return nil, r.error(err)
		}
		if err := permissionsPath.Read(bytes.NewReader(metadataBytes), &permissions); err != nil {
			r.logger.Debug("reading remote schema permissions from metadata", err)
		}

		for idy, role := range permissions {
			contents := role.Definition.Schema
			roleFileName := fmt.Sprintf("%s.graphql", role.Role)
			roleAbsFilePath := filepath.ToSlash(filepath.Join(r.MetadataDir, remoteSchemasDirectory, remoteSchema.Name, remoteSchemasPermissionsDirectory, remoteSchemaPermissionSchemasDirectory, roleFileName))
			files[roleAbsFilePath] = []byte(contents)
			role.Definition.Schema = fmt.Sprintf("%s %s", "!include", filepath.Join(remoteSchemaPermissionSchemasDirectory, roleFileName))
			permissions[idy] = role
		}
		rsPermissionsFile := filepath.Join(remoteSchema.Name, remoteSchemasPermissionsDirectory, remoteSchemaPermissionsFile)
		rsPermissionsFileAbsPath := filepath.ToSlash(filepath.Join(r.MetadataDir, remoteSchemasDirectory, rsPermissionsFile))
		data, err := yaml.Marshal(permissions)
		if err != nil {
			return nil, r.error(err)
		}
		files[rsPermissionsFileAbsPath] = data

		if len(permissions) != 0 {
			rSPermissionIncludeTag := fmt.Sprintf("%s %s", "!include", rsPermissionsFile)
			remoteSchema.Permission = rSPermissionIncludeTag
		}
	}

	data, err := yaml.Marshal(remoteSchemas)
	if err != nil {
		return nil, r.error(err)
	}

	remoteSchemaFile := filepath.Join(r.MetadataDir, remoteSchemasDirectory, r.Filename())
	files[filepath.ToSlash(remoteSchemaFile)] = data
	return files, nil
}

func (r *RemoteSchemaConfig) Key() string {
	return "remote_schemas"
}

func (r *RemoteSchemaConfig) Filename() string {
	return "remote_schemas.yaml"
}
func (r *RemoteSchemaConfig) error(err error, additionalContext ...string) metadataobject.ErrParsingMetadataObject {
	return metadataobject.NewErrParsingMetadataObject(r, err, additionalContext...)
}
