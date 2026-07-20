package metadatautil

import (
	"bytes"
	stderrors "errors"
	"fmt"
	"io"

	"cuelang.org/go/cue/cuecontext"
	"cuelang.org/go/cue/format"
	"cuelang.org/go/encoding/json"
	"cuelang.org/go/encoding/yaml"
	"github.com/hasura/graphql-engine/cli/v2/internal/errors"
	v3yaml "go.yaml.in/yaml/v3"
)

// TODO: reuse https://github.com/hasura/graphql-engine-mono/blob/1c76a0bc4cd0004cc30c17ece672363c142296da/cli/internal/projectmetadata/handler.go#L294
// currently not possible because of the difference in type of struct elements.
// the above mentioned type uses interface{} as types for elements, which will not preserve
// order during unmarshalling. So here we have to tweak it to `yaml.Node`.
type metadata struct {
	Version          v3yaml.Node `mapstructure:"version"                     yaml:"version"`
	Sources          v3yaml.Node `mapstructure:"sources,omitempty"           yaml:"sources,omitempty"`
	Tables           v3yaml.Node `mapstructure:"tables,omitempty"            yaml:"tables,omitempty"`
	Functions        v3yaml.Node `mapstructure:"functions,omitempty"         yaml:"functions,omitempty"`
	Actions          v3yaml.Node `mapstructure:"actions,omitempty"           yaml:"actions,omitempty"`
	CustomTypes      v3yaml.Node `mapstructure:"custom_types,omitempty"      yaml:"custom_types,omitempty"`
	RemoteSchemas    v3yaml.Node `mapstructure:"remote_schemas,omitempty"    yaml:"remote_schemas,omitempty"`
	QueryCollections v3yaml.Node `mapstructure:"query_collections,omitempty" yaml:"query_collections,omitempty"`
	AllowList        v3yaml.Node `mapstructure:"allowlist,omitempty"         yaml:"allowlist,omitempty"`
	CronTriggers     v3yaml.Node `mapstructure:"cron_triggers,omitempty"     yaml:"cron_triggers,omitempty"`
	Network          v3yaml.Node `mapstructure:"network,omitempty"           yaml:"network,omitempty"`
	APILimits        v3yaml.Node `mapstructure:"api_limits,omitempty"        yaml:"api_limits,omitempty"`
	RestEndpoints    v3yaml.Node `mapstructure:"rest_endpoints,omitempty"    yaml:"rest_endpoints,omitempty"`
	InheritedRoles   v3yaml.Node `mapstructure:"inherited_roles,omitempty"   yaml:"inherited_roles,omitempty"`
	Opentelemetry    v3yaml.Node `mapstructure:"opentelemetry,omitempty"     yaml:"opentelemetry,omitempty"`
	BackendConfig    v3yaml.Node `mapstructure:"backend_configs,omitempty"   yaml:"backend_configs,omitempty"`

	// HGE Pro
	GraphQLSchemaIntrospection v3yaml.Node `mapstructure:"graphql_schema_introspection,omitempty" yaml:"graphql_schema_introspection,omitempty"`
	MetricsConfig              v3yaml.Node `mapstructure:"metrics_config,omitempty"               yaml:"metrics_config,omitempty"`

	// note: update projectmetadata/handler.go.Metadata to reflect changes made here
	// TODO: remove this note once the TODO item above (code reuse) is addressed
	// until it is addressed this struct & https://github.com/hasura/graphql-engine-mono/blob/1c76a0bc4cd0004cc30c17ece672363c142296da/cli/internal/projectmetadata/handler.go#L294
	// has to be kept in sync manually.
}

func JSONToYAML(bs []byte) ([]byte, error) {
	var op errors.Op = "metadatautil.JSONToYAML"

	out := new(bytes.Buffer)

	cueJSONDecoder := json.NewDecoder(nil, "", bytes.NewReader(bs))
	for {
		// ref: https://github.com/cue-lang/cue/blob/6bc922c848660781778819a90a343285d0906e2e/encoding/json/json_test.go#L120
		cueExpr, err := cueJSONDecoder.Extract()
		if stderrors.Is(err, io.EOF) {
			break
		}

		if err != nil {
			return nil, errors.E(op, fmt.Errorf("cue: reading metadata %w", err))
		}

		cueNode, err := format.Node(cueExpr)
		if err != nil {
			return nil, errors.E(op, fmt.Errorf("cue: formatting error %w", err))
		}

		_, err = fmt.Fprint(out, string(cueNode))
		if err != nil {
			return nil, errors.E(
				op,
				fmt.Errorf("cue: failed writing parsed json to writer %w", err),
			)
		}
	}

	cueCtx := cuecontext.New()
	cueVal := cueCtx.CompileBytes(out.Bytes())

	// ref: https://github.com/cue-lang/cue/blob/6bc922c848660781778819a90a343285d0906e2e/encoding/yaml/yaml_test.go#L234
	metadataYAML, err := yaml.Encode(cueVal)
	if err != nil {
		return nil, errors.E(op, err)
	}

	// we have to preserve order of elements, so we have to use a datastructure which has that property
	var md metadata

	err = v3yaml.Unmarshal(metadataYAML, &md)
	if err != nil {
		return nil, errors.E(op, err)
	}

	buf := new(bytes.Buffer)

	// TODO: use metadatobject.GetEncoder (currently not possible because of cyclic dependency)
	enc := v3yaml.NewEncoder(buf)
	enc.SetIndent(2)

	err = enc.Encode(md)
	if err != nil {
		return nil, errors.E(op, err)
	}

	return buf.Bytes(), nil
}
