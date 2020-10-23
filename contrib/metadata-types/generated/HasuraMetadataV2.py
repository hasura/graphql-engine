# To use this code, make sure you
#
#     import json
#
# and then, to convert JSON from a string, do
#
#     result = pg_column_from_dict(json.loads(json_string))
#     result = computed_field_name_from_dict(json.loads(json_string))
#     result = role_name_from_dict(json.loads(json_string))
#     result = trigger_name_from_dict(json.loads(json_string))
#     result = remote_relationship_name_from_dict(json.loads(json_string))
#     result = remote_schema_name_from_dict(json.loads(json_string))
#     result = collection_name_from_dict(json.loads(json_string))
#     result = graph_ql_name_from_dict(json.loads(json_string))
#     result = graph_ql_type_from_dict(json.loads(json_string))
#     result = relationship_name_from_dict(json.loads(json_string))
#     result = action_name_from_dict(json.loads(json_string))
#     result = webhook_url_from_dict(json.loads(json_string))
#     result = table_name_from_dict(json.loads(json_string))
#     result = qualified_table_from_dict(json.loads(json_string))
#     result = table_config_from_dict(json.loads(json_string))
#     result = table_entry_from_dict(json.loads(json_string))
#     result = custom_root_fields_from_dict(json.loads(json_string))
#     result = custom_column_names_from_dict(json.loads(json_string))
#     result = function_name_from_dict(json.loads(json_string))
#     result = qualified_function_from_dict(json.loads(json_string))
#     result = custom_function_from_dict(json.loads(json_string))
#     result = function_configuration_from_dict(json.loads(json_string))
#     result = object_relationship_from_dict(json.loads(json_string))
#     result = obj_rel_using_from_dict(json.loads(json_string))
#     result = obj_rel_using_manual_mapping_from_dict(json.loads(json_string))
#     result = array_relationship_from_dict(json.loads(json_string))
#     result = arr_rel_using_from_dict(json.loads(json_string))
#     result = arr_rel_using_f_key_on_from_dict(json.loads(json_string))
#     result = arr_rel_using_manual_mapping_from_dict(json.loads(json_string))
#     result = column_presets_expression_from_dict(json.loads(json_string))
#     result = insert_permission_entry_from_dict(json.loads(json_string))
#     result = insert_permission_from_dict(json.loads(json_string))
#     result = select_permission_entry_from_dict(json.loads(json_string))
#     result = select_permission_from_dict(json.loads(json_string))
#     result = update_permission_entry_from_dict(json.loads(json_string))
#     result = update_permission_from_dict(json.loads(json_string))
#     result = delete_permission_entry_from_dict(json.loads(json_string))
#     result = delete_permission_from_dict(json.loads(json_string))
#     result = computed_field_from_dict(json.loads(json_string))
#     result = computed_field_definition_from_dict(json.loads(json_string))
#     result = event_trigger_from_dict(json.loads(json_string))
#     result = event_trigger_definition_from_dict(json.loads(json_string))
#     result = event_trigger_columns_from_dict(json.loads(json_string))
#     result = operation_spec_from_dict(json.loads(json_string))
#     result = header_from_value_from_dict(json.loads(json_string))
#     result = header_from_env_from_dict(json.loads(json_string))
#     result = retry_conf_from_dict(json.loads(json_string))
#     result = cron_trigger_from_dict(json.loads(json_string))
#     result = retry_conf_st_from_dict(json.loads(json_string))
#     result = remote_schema_from_dict(json.loads(json_string))
#     result = remote_schema_def_from_dict(json.loads(json_string))
#     result = remote_relationship_from_dict(json.loads(json_string))
#     result = remote_relationship_def_from_dict(json.loads(json_string))
#     result = remote_field_from_dict(json.loads(json_string))
#     result = input_arguments_from_dict(json.loads(json_string))
#     result = query_collection_entry_from_dict(json.loads(json_string))
#     result = query_collection_from_dict(json.loads(json_string))
#     result = allow_list_from_dict(json.loads(json_string))
#     result = custom_types_from_dict(json.loads(json_string))
#     result = input_object_type_from_dict(json.loads(json_string))
#     result = input_object_field_from_dict(json.loads(json_string))
#     result = object_type_from_dict(json.loads(json_string))
#     result = object_field_from_dict(json.loads(json_string))
#     result = custom_type_object_relationship_from_dict(json.loads(json_string))
#     result = scalar_type_from_dict(json.loads(json_string))
#     result = enum_type_from_dict(json.loads(json_string))
#     result = enum_value_from_dict(json.loads(json_string))
#     result = action_from_dict(json.loads(json_string))
#     result = action_definition_from_dict(json.loads(json_string))
#     result = input_argument_from_dict(json.loads(json_string))
#     result = hasura_metadata_v2_from_dict(json.loads(json_string))

from dataclasses import dataclass
from typing import Any, Optional, List, Dict, Union, TypeVar, Callable, Type, cast
from enum import Enum


T = TypeVar("T")
EnumT = TypeVar("EnumT", bound=Enum)


def from_str(x: Any) -> str:
    assert isinstance(x, str)
    return x


def from_none(x: Any) -> Any:
    assert x is None
    return x


def from_union(fs, x):
    for f in fs:
        try:
            return f(x)
        except:
            pass
    assert False


def from_list(f: Callable[[Any], T], x: Any) -> List[T]:
    assert isinstance(x, list)
    return [f(y) for y in x]


def from_bool(x: Any) -> bool:
    assert isinstance(x, bool)
    return x


def to_class(c: Type[T], x: Any) -> dict:
    assert isinstance(x, c)
    return cast(Any, x).to_dict()


def to_enum(c: Type[EnumT], x: Any) -> EnumT:
    assert isinstance(x, c)
    return x.value


def from_int(x: Any) -> int:
    assert isinstance(x, int) and not isinstance(x, bool)
    return x


def from_dict(f: Callable[[Any], T], x: Any) -> Dict[str, T]:
    assert isinstance(x, dict)
    return { k: f(v) for (k, v) in x.items() }


def from_float(x: Any) -> float:
    assert isinstance(x, (float, int)) and not isinstance(x, bool)
    return float(x)


def to_float(x: Any) -> float:
    assert isinstance(x, float)
    return x


@dataclass
class HeaderFromValue:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromvalue
    """
    """Name of the header"""
    name: str
    """Value of the header"""
    value: str

    @staticmethod
    def from_dict(obj: Any) -> 'HeaderFromValue':
        assert isinstance(obj, dict)
        name = from_str(obj.get("name"))
        value = from_str(obj.get("value"))
        return HeaderFromValue(name, value)

    def to_dict(self) -> dict:
        result: dict = {}
        result["name"] = from_str(self.name)
        result["value"] = from_str(self.value)
        return result


@dataclass
class HeaderFromEnv:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromenv
    """
    """Name of the header"""
    name: str
    """Name of the environment variable which holds the value of the header"""
    value_from_env: str

    @staticmethod
    def from_dict(obj: Any) -> 'HeaderFromEnv':
        assert isinstance(obj, dict)
        name = from_str(obj.get("name"))
        value_from_env = from_str(obj.get("value_from_env"))
        return HeaderFromEnv(name, value_from_env)

    def to_dict(self) -> dict:
        result: dict = {}
        result["name"] = from_str(self.name)
        result["value_from_env"] = from_str(self.value_from_env)
        return result


@dataclass
class ObjectField:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#objectfield
    """
    """Name of the Input object type"""
    name: str
    """GraphQL type of the Input object type"""
    type: str
    """Description of the Input object type"""
    description: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'ObjectField':
        assert isinstance(obj, dict)
        name = from_str(obj.get("name"))
        type = from_str(obj.get("type"))
        description = from_union([from_str, from_none], obj.get("description"))
        return ObjectField(name, type, description)

    def to_dict(self) -> dict:
        result: dict = {}
        result["name"] = from_str(self.name)
        result["type"] = from_str(self.type)
        result["description"] = from_union([from_str, from_none], self.description)
        return result


@dataclass
class InputArgument:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/actions.html#inputargument
    """
    name: str
    type: str

    @staticmethod
    def from_dict(obj: Any) -> 'InputArgument':
        assert isinstance(obj, dict)
        name = from_str(obj.get("name"))
        type = from_str(obj.get("type"))
        return InputArgument(name, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["name"] = from_str(self.name)
        result["type"] = from_str(self.type)
        return result


@dataclass
class Header:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromvalue
    
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#headerfromenv
    """
    """Name of the header"""
    name: str
    """Value of the header"""
    value: Optional[str] = None
    """Name of the environment variable which holds the value of the header"""
    value_from_env: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'Header':
        assert isinstance(obj, dict)
        name = from_str(obj.get("name"))
        value = from_union([from_str, from_none], obj.get("value"))
        value_from_env = from_union([from_str, from_none], obj.get("value_from_env"))
        return Header(name, value, value_from_env)

    def to_dict(self) -> dict:
        result: dict = {}
        result["name"] = from_str(self.name)
        result["value"] = from_union([from_str, from_none], self.value)
        result["value_from_env"] = from_union([from_str, from_none], self.value_from_env)
        return result


class ActionDefinitionType(Enum):
    MUTATION = "mutation"
    QUERY = "query"


@dataclass
class ActionDefinition:
    """Definition of the action
    
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/actions.html#actiondefinition
    """
    """A String value which supports templating environment variables enclosed in {{ and }}.
    Template example: https://{{ACTION_API_DOMAIN}}/create-user
    """
    handler: str
    arguments: Optional[List[InputArgument]] = None
    forward_client_headers: Optional[bool] = None
    headers: Optional[List[Header]] = None
    kind: Optional[str] = None
    output_type: Optional[str] = None
    type: Optional[ActionDefinitionType] = None

    @staticmethod
    def from_dict(obj: Any) -> 'ActionDefinition':
        assert isinstance(obj, dict)
        handler = from_str(obj.get("handler"))
        arguments = from_union([lambda x: from_list(InputArgument.from_dict, x), from_none], obj.get("arguments"))
        forward_client_headers = from_union([from_bool, from_none], obj.get("forward_client_headers"))
        headers = from_union([lambda x: from_list(Header.from_dict, x), from_none], obj.get("headers"))
        kind = from_union([from_str, from_none], obj.get("kind"))
        output_type = from_union([from_str, from_none], obj.get("output_type"))
        type = from_union([ActionDefinitionType, from_none], obj.get("type"))
        return ActionDefinition(handler, arguments, forward_client_headers, headers, kind, output_type, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["handler"] = from_str(self.handler)
        result["arguments"] = from_union([lambda x: from_list(lambda x: to_class(InputArgument, x), x), from_none], self.arguments)
        result["forward_client_headers"] = from_union([from_bool, from_none], self.forward_client_headers)
        result["headers"] = from_union([lambda x: from_list(lambda x: to_class(Header, x), x), from_none], self.headers)
        result["kind"] = from_union([from_str, from_none], self.kind)
        result["output_type"] = from_union([from_str, from_none], self.output_type)
        result["type"] = from_union([lambda x: to_enum(ActionDefinitionType, x), from_none], self.type)
        return result


@dataclass
class Permissions:
    """Permissions of the action"""
    role: str

    @staticmethod
    def from_dict(obj: Any) -> 'Permissions':
        assert isinstance(obj, dict)
        role = from_str(obj.get("role"))
        return Permissions(role)

    def to_dict(self) -> dict:
        result: dict = {}
        result["role"] = from_str(self.role)
        return result


@dataclass
class Action:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/actions.html#args-syntax
    """
    """Definition of the action"""
    definition: ActionDefinition
    """Name of the action"""
    name: str
    """Comment"""
    comment: Optional[str] = None
    """Permissions of the action"""
    permissions: Optional[Permissions] = None

    @staticmethod
    def from_dict(obj: Any) -> 'Action':
        assert isinstance(obj, dict)
        definition = ActionDefinition.from_dict(obj.get("definition"))
        name = from_str(obj.get("name"))
        comment = from_union([from_str, from_none], obj.get("comment"))
        permissions = from_union([Permissions.from_dict, from_none], obj.get("permissions"))
        return Action(definition, name, comment, permissions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["definition"] = to_class(ActionDefinition, self.definition)
        result["name"] = from_str(self.name)
        result["comment"] = from_union([from_str, from_none], self.comment)
        result["permissions"] = from_union([lambda x: to_class(Permissions, x), from_none], self.permissions)
        return result


@dataclass
class AllowList:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/query-collections.html#add-collection-to-allowlist-syntax
    """
    """Name of a query collection to be added to the allow-list"""
    collection: str

    @staticmethod
    def from_dict(obj: Any) -> 'AllowList':
        assert isinstance(obj, dict)
        collection = from_str(obj.get("collection"))
        return AllowList(collection)

    def to_dict(self) -> dict:
        result: dict = {}
        result["collection"] = from_str(self.collection)
        return result


@dataclass
class RetryConfST:
    """Retry configuration if scheduled invocation delivery fails
    
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/scheduled-triggers.html#retryconfst
    """
    """Number of times to retry delivery.
    Default: 0
    """
    num_retries: Optional[int] = None
    """Number of seconds to wait between each retry.
    Default: 10
    """
    retry_interval_seconds: Optional[int] = None
    """Number of seconds to wait for response before timing out.
    Default: 60
    """
    timeout_seconds: Optional[int] = None
    """Number of seconds between scheduled time and actual delivery time that is acceptable. If
    the time difference is more than this, then the event is dropped.
    Default: 21600 (6 hours)
    """
    tolerance_seconds: Optional[int] = None

    @staticmethod
    def from_dict(obj: Any) -> 'RetryConfST':
        assert isinstance(obj, dict)
        num_retries = from_union([from_int, from_none], obj.get("num_retries"))
        retry_interval_seconds = from_union([from_int, from_none], obj.get("retry_interval_seconds"))
        timeout_seconds = from_union([from_int, from_none], obj.get("timeout_seconds"))
        tolerance_seconds = from_union([from_int, from_none], obj.get("tolerance_seconds"))
        return RetryConfST(num_retries, retry_interval_seconds, timeout_seconds, tolerance_seconds)

    def to_dict(self) -> dict:
        result: dict = {}
        result["num_retries"] = from_union([from_int, from_none], self.num_retries)
        result["retry_interval_seconds"] = from_union([from_int, from_none], self.retry_interval_seconds)
        result["timeout_seconds"] = from_union([from_int, from_none], self.timeout_seconds)
        result["tolerance_seconds"] = from_union([from_int, from_none], self.tolerance_seconds)
        return result


@dataclass
class CronTrigger:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/scheduled-triggers.html#create-cron-trigger
    """
    """List of headers to be sent with the webhook"""
    headers: List[Header]
    """Flag to indicate whether a trigger should be included in the metadata. When a cron
    trigger is included in the metadata, the user will be able to export it when the metadata
    of the graphql-engine is exported.
    """
    include_in_metadata: bool
    """Name of the cron trigger"""
    name: str
    """Cron expression at which the trigger should be invoked."""
    schedule: str
    """URL of the webhook"""
    webhook: str
    """Custom comment."""
    comment: Optional[str] = None
    """Any JSON payload which will be sent when the webhook is invoked."""
    payload: Optional[Dict[str, Any]] = None
    """Retry configuration if scheduled invocation delivery fails"""
    retry_conf: Optional[RetryConfST] = None

    @staticmethod
    def from_dict(obj: Any) -> 'CronTrigger':
        assert isinstance(obj, dict)
        headers = from_list(Header.from_dict, obj.get("headers"))
        include_in_metadata = from_bool(obj.get("include_in_metadata"))
        name = from_str(obj.get("name"))
        schedule = from_str(obj.get("schedule"))
        webhook = from_str(obj.get("webhook"))
        comment = from_union([from_str, from_none], obj.get("comment"))
        payload = from_union([lambda x: from_dict(lambda x: x, x), from_none], obj.get("payload"))
        retry_conf = from_union([RetryConfST.from_dict, from_none], obj.get("retry_conf"))
        return CronTrigger(headers, include_in_metadata, name, schedule, webhook, comment, payload, retry_conf)

    def to_dict(self) -> dict:
        result: dict = {}
        result["headers"] = from_list(lambda x: to_class(Header, x), self.headers)
        result["include_in_metadata"] = from_bool(self.include_in_metadata)
        result["name"] = from_str(self.name)
        result["schedule"] = from_str(self.schedule)
        result["webhook"] = from_str(self.webhook)
        result["comment"] = from_union([from_str, from_none], self.comment)
        result["payload"] = from_union([lambda x: from_dict(lambda x: x, x), from_none], self.payload)
        result["retry_conf"] = from_union([lambda x: to_class(RetryConfST, x), from_none], self.retry_conf)
        return result


@dataclass
class EnumValue:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#enumvalue
    """
    """Value of the Enum type"""
    value: str
    """Description of the Enum value"""
    description: Optional[str] = None
    """If set to true, the enum value is marked as deprecated"""
    is_deprecated: Optional[bool] = None

    @staticmethod
    def from_dict(obj: Any) -> 'EnumValue':
        assert isinstance(obj, dict)
        value = from_str(obj.get("value"))
        description = from_union([from_str, from_none], obj.get("description"))
        is_deprecated = from_union([from_bool, from_none], obj.get("is_deprecated"))
        return EnumValue(value, description, is_deprecated)

    def to_dict(self) -> dict:
        result: dict = {}
        result["value"] = from_str(self.value)
        result["description"] = from_union([from_str, from_none], self.description)
        result["is_deprecated"] = from_union([from_bool, from_none], self.is_deprecated)
        return result


@dataclass
class EnumType:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#enumtype
    """
    """Name of the Enum type"""
    name: str
    """Values of the Enum type"""
    values: List[EnumValue]
    """Description of the Enum type"""
    description: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'EnumType':
        assert isinstance(obj, dict)
        name = from_str(obj.get("name"))
        values = from_list(EnumValue.from_dict, obj.get("values"))
        description = from_union([from_str, from_none], obj.get("description"))
        return EnumType(name, values, description)

    def to_dict(self) -> dict:
        result: dict = {}
        result["name"] = from_str(self.name)
        result["values"] = from_list(lambda x: to_class(EnumValue, x), self.values)
        result["description"] = from_union([from_str, from_none], self.description)
        return result


@dataclass
class InputObjectField:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#inputobjectfield
    """
    """Name of the Input object type"""
    name: str
    """GraphQL type of the Input object type"""
    type: str
    """Description of the Input object type"""
    description: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'InputObjectField':
        assert isinstance(obj, dict)
        name = from_str(obj.get("name"))
        type = from_str(obj.get("type"))
        description = from_union([from_str, from_none], obj.get("description"))
        return InputObjectField(name, type, description)

    def to_dict(self) -> dict:
        result: dict = {}
        result["name"] = from_str(self.name)
        result["type"] = from_str(self.type)
        result["description"] = from_union([from_str, from_none], self.description)
        return result


@dataclass
class InputObjectType:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#inputobjecttype
    """
    """Fields of the Input object type"""
    fields: List[InputObjectField]
    """Name of the Input object type"""
    name: str
    """Description of the Input object type"""
    description: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'InputObjectType':
        assert isinstance(obj, dict)
        fields = from_list(InputObjectField.from_dict, obj.get("fields"))
        name = from_str(obj.get("name"))
        description = from_union([from_str, from_none], obj.get("description"))
        return InputObjectType(fields, name, description)

    def to_dict(self) -> dict:
        result: dict = {}
        result["fields"] = from_list(lambda x: to_class(InputObjectField, x), self.fields)
        result["name"] = from_str(self.name)
        result["description"] = from_union([from_str, from_none], self.description)
        return result


@dataclass
class QualifiedTable:
    name: str
    schema: str

    @staticmethod
    def from_dict(obj: Any) -> 'QualifiedTable':
        assert isinstance(obj, dict)
        name = from_str(obj.get("name"))
        schema = from_str(obj.get("schema"))
        return QualifiedTable(name, schema)

    def to_dict(self) -> dict:
        result: dict = {}
        result["name"] = from_str(self.name)
        result["schema"] = from_str(self.schema)
        return result


class CustomTypeObjectRelationshipType(Enum):
    """Type of the relationship"""
    ARRAY = "array"
    OBJECT = "object"


@dataclass
class CustomTypeObjectRelationship:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#objectrelationship
    """
    """Mapping of fields of object type to columns of remote table"""
    field_mapping: Dict[str, str]
    """Name of the relationship, shouldn’t conflict with existing field names"""
    name: str
    """The table to which relationship is defined"""
    remote_table: Union[QualifiedTable, str]
    """Type of the relationship"""
    type: CustomTypeObjectRelationshipType

    @staticmethod
    def from_dict(obj: Any) -> 'CustomTypeObjectRelationship':
        assert isinstance(obj, dict)
        field_mapping = from_dict(from_str, obj.get("field_mapping"))
        name = from_str(obj.get("name"))
        remote_table = from_union([QualifiedTable.from_dict, from_str], obj.get("remote_table"))
        type = CustomTypeObjectRelationshipType(obj.get("type"))
        return CustomTypeObjectRelationship(field_mapping, name, remote_table, type)

    def to_dict(self) -> dict:
        result: dict = {}
        result["field_mapping"] = from_dict(from_str, self.field_mapping)
        result["name"] = from_str(self.name)
        result["remote_table"] = from_union([lambda x: to_class(QualifiedTable, x), from_str], self.remote_table)
        result["type"] = to_enum(CustomTypeObjectRelationshipType, self.type)
        return result


@dataclass
class ObjectType:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#objecttype
    """
    """Fields of the Input object type"""
    fields: List[InputObjectField]
    """Name of the Input object type"""
    name: str
    """Description of the Input object type"""
    description: Optional[str] = None
    """Relationships of the Object type to tables"""
    relationships: Optional[List[CustomTypeObjectRelationship]] = None

    @staticmethod
    def from_dict(obj: Any) -> 'ObjectType':
        assert isinstance(obj, dict)
        fields = from_list(InputObjectField.from_dict, obj.get("fields"))
        name = from_str(obj.get("name"))
        description = from_union([from_str, from_none], obj.get("description"))
        relationships = from_union([lambda x: from_list(CustomTypeObjectRelationship.from_dict, x), from_none], obj.get("relationships"))
        return ObjectType(fields, name, description, relationships)

    def to_dict(self) -> dict:
        result: dict = {}
        result["fields"] = from_list(lambda x: to_class(InputObjectField, x), self.fields)
        result["name"] = from_str(self.name)
        result["description"] = from_union([from_str, from_none], self.description)
        result["relationships"] = from_union([lambda x: from_list(lambda x: to_class(CustomTypeObjectRelationship, x), x), from_none], self.relationships)
        return result


@dataclass
class ScalarType:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-types.html#scalartype
    """
    """Name of the Scalar type"""
    name: str
    """Description of the Scalar type"""
    description: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'ScalarType':
        assert isinstance(obj, dict)
        name = from_str(obj.get("name"))
        description = from_union([from_str, from_none], obj.get("description"))
        return ScalarType(name, description)

    def to_dict(self) -> dict:
        result: dict = {}
        result["name"] = from_str(self.name)
        result["description"] = from_union([from_str, from_none], self.description)
        return result


@dataclass
class CustomTypes:
    enums: Optional[List[EnumType]] = None
    input_objects: Optional[List[InputObjectType]] = None
    objects: Optional[List[ObjectType]] = None
    scalars: Optional[List[ScalarType]] = None

    @staticmethod
    def from_dict(obj: Any) -> 'CustomTypes':
        assert isinstance(obj, dict)
        enums = from_union([lambda x: from_list(EnumType.from_dict, x), from_none], obj.get("enums"))
        input_objects = from_union([lambda x: from_list(InputObjectType.from_dict, x), from_none], obj.get("input_objects"))
        objects = from_union([lambda x: from_list(ObjectType.from_dict, x), from_none], obj.get("objects"))
        scalars = from_union([lambda x: from_list(ScalarType.from_dict, x), from_none], obj.get("scalars"))
        return CustomTypes(enums, input_objects, objects, scalars)

    def to_dict(self) -> dict:
        result: dict = {}
        result["enums"] = from_union([lambda x: from_list(lambda x: to_class(EnumType, x), x), from_none], self.enums)
        result["input_objects"] = from_union([lambda x: from_list(lambda x: to_class(InputObjectType, x), x), from_none], self.input_objects)
        result["objects"] = from_union([lambda x: from_list(lambda x: to_class(ObjectType, x), x), from_none], self.objects)
        result["scalars"] = from_union([lambda x: from_list(lambda x: to_class(ScalarType, x), x), from_none], self.scalars)
        return result


@dataclass
class FunctionConfiguration:
    """Configuration for the SQL function
    
    Configuration for a CustomFunction
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-functions.html#function-configuration
    """
    """Function argument which accepts session info JSON
    Currently, only functions which satisfy the following constraints can be exposed over the
    GraphQL API (terminology from Postgres docs):
    - Function behaviour: ONLY `STABLE` or `IMMUTABLE`
    - Return type: MUST be `SETOF <table-name>`
    - Argument modes: ONLY `IN`
    """
    session_argument: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'FunctionConfiguration':
        assert isinstance(obj, dict)
        session_argument = from_union([from_str, from_none], obj.get("session_argument"))
        return FunctionConfiguration(session_argument)

    def to_dict(self) -> dict:
        result: dict = {}
        result["session_argument"] = from_union([from_str, from_none], self.session_argument)
        return result


@dataclass
class QualifiedFunction:
    name: str
    schema: str

    @staticmethod
    def from_dict(obj: Any) -> 'QualifiedFunction':
        assert isinstance(obj, dict)
        name = from_str(obj.get("name"))
        schema = from_str(obj.get("schema"))
        return QualifiedFunction(name, schema)

    def to_dict(self) -> dict:
        result: dict = {}
        result["name"] = from_str(self.name)
        result["schema"] = from_str(self.schema)
        return result


@dataclass
class CustomFunction:
    """A custom SQL function to add to the GraphQL schema with configuration.
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/custom-functions.html#args-syntax
    """
    """Name of the SQL function"""
    function: Union[QualifiedFunction, str]
    """Configuration for the SQL function"""
    configuration: Optional[FunctionConfiguration] = None

    @staticmethod
    def from_dict(obj: Any) -> 'CustomFunction':
        assert isinstance(obj, dict)
        function = from_union([QualifiedFunction.from_dict, from_str], obj.get("function"))
        configuration = from_union([FunctionConfiguration.from_dict, from_none], obj.get("configuration"))
        return CustomFunction(function, configuration)

    def to_dict(self) -> dict:
        result: dict = {}
        result["function"] = from_union([lambda x: to_class(QualifiedFunction, x), from_str], self.function)
        result["configuration"] = from_union([lambda x: to_class(FunctionConfiguration, x), from_none], self.configuration)
        return result


@dataclass
class QueryCollection:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#collectionquery
    """
    name: str
    query: str

    @staticmethod
    def from_dict(obj: Any) -> 'QueryCollection':
        assert isinstance(obj, dict)
        name = from_str(obj.get("name"))
        query = from_str(obj.get("query"))
        return QueryCollection(name, query)

    def to_dict(self) -> dict:
        result: dict = {}
        result["name"] = from_str(self.name)
        result["query"] = from_str(self.query)
        return result


@dataclass
class Definition:
    """List of queries"""
    queries: List[QueryCollection]

    @staticmethod
    def from_dict(obj: Any) -> 'Definition':
        assert isinstance(obj, dict)
        queries = from_list(QueryCollection.from_dict, obj.get("queries"))
        return Definition(queries)

    def to_dict(self) -> dict:
        result: dict = {}
        result["queries"] = from_list(lambda x: to_class(QueryCollection, x), self.queries)
        return result


@dataclass
class QueryCollectionEntry:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/query-collections.html#args-syntax
    """
    """List of queries"""
    definition: Definition
    """Name of the query collection"""
    name: str
    """Comment"""
    comment: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'QueryCollectionEntry':
        assert isinstance(obj, dict)
        definition = Definition.from_dict(obj.get("definition"))
        name = from_str(obj.get("name"))
        comment = from_union([from_str, from_none], obj.get("comment"))
        return QueryCollectionEntry(definition, name, comment)

    def to_dict(self) -> dict:
        result: dict = {}
        result["definition"] = to_class(Definition, self.definition)
        result["name"] = from_str(self.name)
        result["comment"] = from_union([from_str, from_none], self.comment)
        return result


@dataclass
class RemoteSchemaDef:
    """Name of the remote schema
    
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/syntax-defs.html#remoteschemadef
    """
    forward_client_headers: Optional[bool] = None
    headers: Optional[List[Header]] = None
    timeout_seconds: Optional[float] = None
    url: Optional[str] = None
    url_from_env: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'RemoteSchemaDef':
        assert isinstance(obj, dict)
        forward_client_headers = from_union([from_bool, from_none], obj.get("forward_client_headers"))
        headers = from_union([lambda x: from_list(Header.from_dict, x), from_none], obj.get("headers"))
        timeout_seconds = from_union([from_float, from_none], obj.get("timeout_seconds"))
        url = from_union([from_str, from_none], obj.get("url"))
        url_from_env = from_union([from_str, from_none], obj.get("url_from_env"))
        return RemoteSchemaDef(forward_client_headers, headers, timeout_seconds, url, url_from_env)

    def to_dict(self) -> dict:
        result: dict = {}
        result["forward_client_headers"] = from_union([from_bool, from_none], self.forward_client_headers)
        result["headers"] = from_union([lambda x: from_list(lambda x: to_class(Header, x), x), from_none], self.headers)
        result["timeout_seconds"] = from_union([to_float, from_none], self.timeout_seconds)
        result["url"] = from_union([from_str, from_none], self.url)
        result["url_from_env"] = from_union([from_str, from_none], self.url_from_env)
        return result


@dataclass
class RemoteSchema:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/remote-schemas.html#add-remote-schema
    """
    """Name of the remote schema"""
    definition: RemoteSchemaDef
    """Name of the remote schema"""
    name: str
    """Comment"""
    comment: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'RemoteSchema':
        assert isinstance(obj, dict)
        definition = RemoteSchemaDef.from_dict(obj.get("definition"))
        name = from_str(obj.get("name"))
        comment = from_union([from_str, from_none], obj.get("comment"))
        return RemoteSchema(definition, name, comment)

    def to_dict(self) -> dict:
        result: dict = {}
        result["definition"] = to_class(RemoteSchemaDef, self.definition)
        result["name"] = from_str(self.name)
        result["comment"] = from_union([from_str, from_none], self.comment)
        return result


@dataclass
class ArrRelUsingFKeyOn:
    """The column with foreign key constraint
    
    The column with foreign key constraint
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#arrrelusingfkeyon
    """
    column: str
    table: Union[QualifiedTable, str]

    @staticmethod
    def from_dict(obj: Any) -> 'ArrRelUsingFKeyOn':
        assert isinstance(obj, dict)
        column = from_str(obj.get("column"))
        table = from_union([QualifiedTable.from_dict, from_str], obj.get("table"))
        return ArrRelUsingFKeyOn(column, table)

    def to_dict(self) -> dict:
        result: dict = {}
        result["column"] = from_str(self.column)
        result["table"] = from_union([lambda x: to_class(QualifiedTable, x), from_str], self.table)
        return result


@dataclass
class ArrRelUsingManualMapping:
    """Manual mapping of table and columns
    
    Manual mapping of table and columns
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#arrrelusingmanualmapping
    """
    """Mapping of columns from current table to remote table"""
    column_mapping: Dict[str, str]
    """The table to which the relationship has to be established"""
    remote_table: Union[QualifiedTable, str]

    @staticmethod
    def from_dict(obj: Any) -> 'ArrRelUsingManualMapping':
        assert isinstance(obj, dict)
        column_mapping = from_dict(from_str, obj.get("column_mapping"))
        remote_table = from_union([QualifiedTable.from_dict, from_str], obj.get("remote_table"))
        return ArrRelUsingManualMapping(column_mapping, remote_table)

    def to_dict(self) -> dict:
        result: dict = {}
        result["column_mapping"] = from_dict(from_str, self.column_mapping)
        result["remote_table"] = from_union([lambda x: to_class(QualifiedTable, x), from_str], self.remote_table)
        return result


@dataclass
class ArrRelUsing:
    """Use one of the available ways to define an array relationship
    
    Use one of the available ways to define an object relationship
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#arrrelusing
    """
    """The column with foreign key constraint"""
    foreign_key_constraint_on: Optional[ArrRelUsingFKeyOn] = None
    """Manual mapping of table and columns"""
    manual_configuration: Optional[ArrRelUsingManualMapping] = None

    @staticmethod
    def from_dict(obj: Any) -> 'ArrRelUsing':
        assert isinstance(obj, dict)
        foreign_key_constraint_on = from_union([ArrRelUsingFKeyOn.from_dict, from_none], obj.get("foreign_key_constraint_on"))
        manual_configuration = from_union([ArrRelUsingManualMapping.from_dict, from_none], obj.get("manual_configuration"))
        return ArrRelUsing(foreign_key_constraint_on, manual_configuration)

    def to_dict(self) -> dict:
        result: dict = {}
        result["foreign_key_constraint_on"] = from_union([lambda x: to_class(ArrRelUsingFKeyOn, x), from_none], self.foreign_key_constraint_on)
        result["manual_configuration"] = from_union([lambda x: to_class(ArrRelUsingManualMapping, x), from_none], self.manual_configuration)
        return result


@dataclass
class ArrayRelationship:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#create-array-relationship-syntax
    """
    """Name of the new relationship"""
    name: str
    """Use one of the available ways to define an array relationship"""
    using: ArrRelUsing
    """Comment"""
    comment: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'ArrayRelationship':
        assert isinstance(obj, dict)
        name = from_str(obj.get("name"))
        using = ArrRelUsing.from_dict(obj.get("using"))
        comment = from_union([from_str, from_none], obj.get("comment"))
        return ArrayRelationship(name, using, comment)

    def to_dict(self) -> dict:
        result: dict = {}
        result["name"] = from_str(self.name)
        result["using"] = to_class(ArrRelUsing, self.using)
        result["comment"] = from_union([from_str, from_none], self.comment)
        return result


@dataclass
class ComputedFieldDefinition:
    """The computed field definition
    
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/computed-field.html#computedfielddefinition
    """
    """The SQL function"""
    function: Union[QualifiedFunction, str]
    """Name of the argument which accepts the Hasura session object as a JSON/JSONB value. If
    omitted, the Hasura session object is not passed to the function
    """
    session_argument: Optional[str] = None
    """Name of the argument which accepts a table row type. If omitted, the first argument is
    considered a table argument
    """
    table_argument: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'ComputedFieldDefinition':
        assert isinstance(obj, dict)
        function = from_union([QualifiedFunction.from_dict, from_str], obj.get("function"))
        session_argument = from_union([from_str, from_none], obj.get("session_argument"))
        table_argument = from_union([from_str, from_none], obj.get("table_argument"))
        return ComputedFieldDefinition(function, session_argument, table_argument)

    def to_dict(self) -> dict:
        result: dict = {}
        result["function"] = from_union([lambda x: to_class(QualifiedFunction, x), from_str], self.function)
        result["session_argument"] = from_union([from_str, from_none], self.session_argument)
        result["table_argument"] = from_union([from_str, from_none], self.table_argument)
        return result


@dataclass
class ComputedField:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/computed-field.html#args-syntax
    """
    """The computed field definition"""
    definition: ComputedFieldDefinition
    """Name of the new computed field"""
    name: str
    """Comment"""
    comment: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'ComputedField':
        assert isinstance(obj, dict)
        definition = ComputedFieldDefinition.from_dict(obj.get("definition"))
        name = from_str(obj.get("name"))
        comment = from_union([from_str, from_none], obj.get("comment"))
        return ComputedField(definition, name, comment)

    def to_dict(self) -> dict:
        result: dict = {}
        result["definition"] = to_class(ComputedFieldDefinition, self.definition)
        result["name"] = from_str(self.name)
        result["comment"] = from_union([from_str, from_none], self.comment)
        return result


@dataclass
class CustomRootFields:
    """Customise the root fields
    
    Customise the root fields
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/table-view.html#custom-root-fields
    """
    """Customise the `delete_<table-name>` root field"""
    delete: Optional[str] = None
    """Customise the `delete_<table-name>_by_pk` root field"""
    delete_by_pk: Optional[str] = None
    """Customise the `insert_<table-name>` root field"""
    insert: Optional[str] = None
    """Customise the `insert_<table-name>_one` root field"""
    insert_one: Optional[str] = None
    """Customise the `<table-name>` root field"""
    select: Optional[str] = None
    """Customise the `<table-name>_aggregate` root field"""
    select_aggregate: Optional[str] = None
    """Customise the `<table-name>_by_pk` root field"""
    select_by_pk: Optional[str] = None
    """Customise the `update_<table-name>` root field"""
    update: Optional[str] = None
    """Customise the `update_<table-name>_by_pk` root field"""
    update_by_pk: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'CustomRootFields':
        assert isinstance(obj, dict)
        delete = from_union([from_str, from_none], obj.get("delete"))
        delete_by_pk = from_union([from_str, from_none], obj.get("delete_by_pk"))
        insert = from_union([from_str, from_none], obj.get("insert"))
        insert_one = from_union([from_str, from_none], obj.get("insert_one"))
        select = from_union([from_str, from_none], obj.get("select"))
        select_aggregate = from_union([from_str, from_none], obj.get("select_aggregate"))
        select_by_pk = from_union([from_str, from_none], obj.get("select_by_pk"))
        update = from_union([from_str, from_none], obj.get("update"))
        update_by_pk = from_union([from_str, from_none], obj.get("update_by_pk"))
        return CustomRootFields(delete, delete_by_pk, insert, insert_one, select, select_aggregate, select_by_pk, update, update_by_pk)

    def to_dict(self) -> dict:
        result: dict = {}
        result["delete"] = from_union([from_str, from_none], self.delete)
        result["delete_by_pk"] = from_union([from_str, from_none], self.delete_by_pk)
        result["insert"] = from_union([from_str, from_none], self.insert)
        result["insert_one"] = from_union([from_str, from_none], self.insert_one)
        result["select"] = from_union([from_str, from_none], self.select)
        result["select_aggregate"] = from_union([from_str, from_none], self.select_aggregate)
        result["select_by_pk"] = from_union([from_str, from_none], self.select_by_pk)
        result["update"] = from_union([from_str, from_none], self.update)
        result["update_by_pk"] = from_union([from_str, from_none], self.update_by_pk)
        return result


@dataclass
class TableConfig:
    """Configuration for the table/view
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/table-view.html#table-config
    """
    """Customise the column names"""
    custom_column_names: Optional[Dict[str, str]] = None
    """Customise the root fields"""
    custom_root_fields: Optional[CustomRootFields] = None

    @staticmethod
    def from_dict(obj: Any) -> 'TableConfig':
        assert isinstance(obj, dict)
        custom_column_names = from_union([lambda x: from_dict(from_str, x), from_none], obj.get("custom_column_names"))
        custom_root_fields = from_union([CustomRootFields.from_dict, from_none], obj.get("custom_root_fields"))
        return TableConfig(custom_column_names, custom_root_fields)

    def to_dict(self) -> dict:
        result: dict = {}
        result["custom_column_names"] = from_union([lambda x: from_dict(from_str, x), from_none], self.custom_column_names)
        result["custom_root_fields"] = from_union([lambda x: to_class(CustomRootFields, x), from_none], self.custom_root_fields)
        return result


@dataclass
class DeletePermission:
    """The permission definition
    
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#deletepermission
    """
    """Only the rows where this precondition holds true are updatable"""
    filter: Optional[Dict[str, Union[float, Dict[str, Any], str]]] = None

    @staticmethod
    def from_dict(obj: Any) -> 'DeletePermission':
        assert isinstance(obj, dict)
        filter = from_union([lambda x: from_dict(lambda x: from_union([lambda x: from_dict(lambda x: x, x), from_float, from_str], x), x), from_none], obj.get("filter"))
        return DeletePermission(filter)

    def to_dict(self) -> dict:
        result: dict = {}
        result["filter"] = from_union([lambda x: from_dict(lambda x: from_union([lambda x: from_dict(lambda x: x, x), to_float, from_str], x), x), from_none], self.filter)
        return result


@dataclass
class DeletePermissionEntry:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#create-delete-permission-syntax
    """
    """The permission definition"""
    permission: DeletePermission
    """Role"""
    role: str
    """Comment"""
    comment: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'DeletePermissionEntry':
        assert isinstance(obj, dict)
        permission = DeletePermission.from_dict(obj.get("permission"))
        role = from_str(obj.get("role"))
        comment = from_union([from_str, from_none], obj.get("comment"))
        return DeletePermissionEntry(permission, role, comment)

    def to_dict(self) -> dict:
        result: dict = {}
        result["permission"] = to_class(DeletePermission, self.permission)
        result["role"] = from_str(self.role)
        result["comment"] = from_union([from_str, from_none], self.comment)
        return result


class EventTriggerColumnsEnum(Enum):
    EMPTY = "*"


@dataclass
class OperationSpec:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec
    """
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#eventtriggercolumns
    """
    columns: Union[List[str], EventTriggerColumnsEnum]
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#eventtriggercolumns
    """
    payload: Union[List[str], EventTriggerColumnsEnum, None]

    @staticmethod
    def from_dict(obj: Any) -> 'OperationSpec':
        assert isinstance(obj, dict)
        columns = from_union([lambda x: from_list(from_str, x), EventTriggerColumnsEnum], obj.get("columns"))
        payload = from_union([lambda x: from_list(from_str, x), EventTriggerColumnsEnum, from_none], obj.get("payload"))
        return OperationSpec(columns, payload)

    def to_dict(self) -> dict:
        result: dict = {}
        result["columns"] = from_union([lambda x: from_list(from_str, x), lambda x: to_enum(EventTriggerColumnsEnum, x)], self.columns)
        result["payload"] = from_union([lambda x: from_list(from_str, x), lambda x: to_enum(EventTriggerColumnsEnum, x), from_none], self.payload)
        return result


@dataclass
class EventTriggerDefinition:
    """The SQL function"""
    enable_manual: bool
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec
    """
    delete: Optional[OperationSpec] = None
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec
    """
    insert: Optional[OperationSpec] = None
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#operationspec
    """
    update: Optional[OperationSpec] = None

    @staticmethod
    def from_dict(obj: Any) -> 'EventTriggerDefinition':
        assert isinstance(obj, dict)
        enable_manual = from_bool(obj.get("enable_manual"))
        delete = from_union([OperationSpec.from_dict, from_none], obj.get("delete"))
        insert = from_union([OperationSpec.from_dict, from_none], obj.get("insert"))
        update = from_union([OperationSpec.from_dict, from_none], obj.get("update"))
        return EventTriggerDefinition(enable_manual, delete, insert, update)

    def to_dict(self) -> dict:
        result: dict = {}
        result["enable_manual"] = from_bool(self.enable_manual)
        result["delete"] = from_union([lambda x: to_class(OperationSpec, x), from_none], self.delete)
        result["insert"] = from_union([lambda x: to_class(OperationSpec, x), from_none], self.insert)
        result["update"] = from_union([lambda x: to_class(OperationSpec, x), from_none], self.update)
        return result


@dataclass
class RetryConf:
    """The SQL function
    
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#retryconf
    """
    """Number of seconds to wait between each retry.
    Default: 10
    """
    interval_sec: Optional[int] = None
    """Number of times to retry delivery.
    Default: 0
    """
    num_retries: Optional[int] = None
    """Number of seconds to wait for response before timing out.
    Default: 60
    """
    timeout_sec: Optional[int] = None

    @staticmethod
    def from_dict(obj: Any) -> 'RetryConf':
        assert isinstance(obj, dict)
        interval_sec = from_union([from_int, from_none], obj.get("interval_sec"))
        num_retries = from_union([from_int, from_none], obj.get("num_retries"))
        timeout_sec = from_union([from_int, from_none], obj.get("timeout_sec"))
        return RetryConf(interval_sec, num_retries, timeout_sec)

    def to_dict(self) -> dict:
        result: dict = {}
        result["interval_sec"] = from_union([from_int, from_none], self.interval_sec)
        result["num_retries"] = from_union([from_int, from_none], self.num_retries)
        result["timeout_sec"] = from_union([from_int, from_none], self.timeout_sec)
        return result


@dataclass
class EventTrigger:
    """NOTE: The metadata type doesn't QUITE match the 'create' arguments here
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/event-triggers.html#create-event-trigger
    """
    """The SQL function"""
    definition: EventTriggerDefinition
    """Name of the event trigger"""
    name: str
    """The SQL function"""
    retry_conf: RetryConf
    """The SQL function"""
    headers: Optional[List[Header]] = None
    """The SQL function"""
    webhook: Optional[str] = None
    webhook_from_env: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'EventTrigger':
        assert isinstance(obj, dict)
        definition = EventTriggerDefinition.from_dict(obj.get("definition"))
        name = from_str(obj.get("name"))
        retry_conf = RetryConf.from_dict(obj.get("retry_conf"))
        headers = from_union([lambda x: from_list(Header.from_dict, x), from_none], obj.get("headers"))
        webhook = from_union([from_str, from_none], obj.get("webhook"))
        webhook_from_env = from_union([from_str, from_none], obj.get("webhook_from_env"))
        return EventTrigger(definition, name, retry_conf, headers, webhook, webhook_from_env)

    def to_dict(self) -> dict:
        result: dict = {}
        result["definition"] = to_class(EventTriggerDefinition, self.definition)
        result["name"] = from_str(self.name)
        result["retry_conf"] = to_class(RetryConf, self.retry_conf)
        result["headers"] = from_union([lambda x: from_list(lambda x: to_class(Header, x), x), from_none], self.headers)
        result["webhook"] = from_union([from_str, from_none], self.webhook)
        result["webhook_from_env"] = from_union([from_str, from_none], self.webhook_from_env)
        return result


@dataclass
class InsertPermission:
    """The permission definition
    
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#insertpermission
    """
    """Can insert into only these columns (or all when '*' is specified)"""
    columns: Union[List[str], EventTriggerColumnsEnum]
    """When set to true the mutation is accessible only if x-hasura-use-backend-only-permissions
    session variable exists
    and is set to true and request is made with x-hasura-admin-secret set if any auth is
    configured
    """
    backend_only: Optional[bool] = None
    """This expression has to hold true for every new row that is inserted"""
    check: Optional[Dict[str, Union[float, Dict[str, Any], str]]] = None
    """Preset values for columns that can be sourced from session variables or static values"""
    set: Optional[Dict[str, str]] = None

    @staticmethod
    def from_dict(obj: Any) -> 'InsertPermission':
        assert isinstance(obj, dict)
        columns = from_union([lambda x: from_list(from_str, x), EventTriggerColumnsEnum], obj.get("columns"))
        backend_only = from_union([from_bool, from_none], obj.get("backend_only"))
        check = from_union([lambda x: from_dict(lambda x: from_union([lambda x: from_dict(lambda x: x, x), from_float, from_str], x), x), from_none], obj.get("check"))
        set = from_union([lambda x: from_dict(from_str, x), from_none], obj.get("set"))
        return InsertPermission(columns, backend_only, check, set)

    def to_dict(self) -> dict:
        result: dict = {}
        result["columns"] = from_union([lambda x: from_list(from_str, x), lambda x: to_enum(EventTriggerColumnsEnum, x)], self.columns)
        result["backend_only"] = from_union([from_bool, from_none], self.backend_only)
        result["check"] = from_union([lambda x: from_dict(lambda x: from_union([lambda x: from_dict(lambda x: x, x), to_float, from_str], x), x), from_none], self.check)
        result["set"] = from_union([lambda x: from_dict(from_str, x), from_none], self.set)
        return result


@dataclass
class InsertPermissionEntry:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#args-syntax
    """
    """The permission definition"""
    permission: InsertPermission
    """Role"""
    role: str
    """Comment"""
    comment: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'InsertPermissionEntry':
        assert isinstance(obj, dict)
        permission = InsertPermission.from_dict(obj.get("permission"))
        role = from_str(obj.get("role"))
        comment = from_union([from_str, from_none], obj.get("comment"))
        return InsertPermissionEntry(permission, role, comment)

    def to_dict(self) -> dict:
        result: dict = {}
        result["permission"] = to_class(InsertPermission, self.permission)
        result["role"] = from_str(self.role)
        result["comment"] = from_union([from_str, from_none], self.comment)
        return result


@dataclass
class ObjRelUsingManualMapping:
    """Manual mapping of table and columns
    
    Manual mapping of table and columns
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#objrelusingmanualmapping
    """
    """Mapping of columns from current table to remote table"""
    column_mapping: Dict[str, str]
    """The table to which the relationship has to be established"""
    remote_table: Union[QualifiedTable, str]

    @staticmethod
    def from_dict(obj: Any) -> 'ObjRelUsingManualMapping':
        assert isinstance(obj, dict)
        column_mapping = from_dict(from_str, obj.get("column_mapping"))
        remote_table = from_union([QualifiedTable.from_dict, from_str], obj.get("remote_table"))
        return ObjRelUsingManualMapping(column_mapping, remote_table)

    def to_dict(self) -> dict:
        result: dict = {}
        result["column_mapping"] = from_dict(from_str, self.column_mapping)
        result["remote_table"] = from_union([lambda x: to_class(QualifiedTable, x), from_str], self.remote_table)
        return result


@dataclass
class ObjRelUsing:
    """Use one of the available ways to define an object relationship
    
    Use one of the available ways to define an object relationship
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#objrelusing
    """
    """The column with foreign key constraint"""
    foreign_key_constraint_on: Optional[str] = None
    """Manual mapping of table and columns"""
    manual_configuration: Optional[ObjRelUsingManualMapping] = None

    @staticmethod
    def from_dict(obj: Any) -> 'ObjRelUsing':
        assert isinstance(obj, dict)
        foreign_key_constraint_on = from_union([from_str, from_none], obj.get("foreign_key_constraint_on"))
        manual_configuration = from_union([ObjRelUsingManualMapping.from_dict, from_none], obj.get("manual_configuration"))
        return ObjRelUsing(foreign_key_constraint_on, manual_configuration)

    def to_dict(self) -> dict:
        result: dict = {}
        result["foreign_key_constraint_on"] = from_union([from_str, from_none], self.foreign_key_constraint_on)
        result["manual_configuration"] = from_union([lambda x: to_class(ObjRelUsingManualMapping, x), from_none], self.manual_configuration)
        return result


@dataclass
class ObjectRelationship:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/relationship.html#args-syntax
    """
    """Name of the new relationship"""
    name: str
    """Use one of the available ways to define an object relationship"""
    using: ObjRelUsing
    """Comment"""
    comment: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'ObjectRelationship':
        assert isinstance(obj, dict)
        name = from_str(obj.get("name"))
        using = ObjRelUsing.from_dict(obj.get("using"))
        comment = from_union([from_str, from_none], obj.get("comment"))
        return ObjectRelationship(name, using, comment)

    def to_dict(self) -> dict:
        result: dict = {}
        result["name"] = from_str(self.name)
        result["using"] = to_class(ObjRelUsing, self.using)
        result["comment"] = from_union([from_str, from_none], self.comment)
        return result


@dataclass
class RemoteFieldValue:
    arguments: Dict[str, str]
    """A recursive tree structure that points to the field in the remote schema that needs to be
    joined with.
    It is recursive because the remote field maybe nested deeply in the remote schema.
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/remote-relationships.html#remotefield
    """
    field: Optional[Dict[str, 'RemoteFieldValue']] = None

    @staticmethod
    def from_dict(obj: Any) -> 'RemoteFieldValue':
        assert isinstance(obj, dict)
        arguments = from_dict(from_str, obj.get("arguments"))
        field = from_union([lambda x: from_dict(RemoteFieldValue.from_dict, x), from_none], obj.get("field"))
        return RemoteFieldValue(arguments, field)

    def to_dict(self) -> dict:
        result: dict = {}
        result["arguments"] = from_dict(from_str, self.arguments)
        result["field"] = from_union([lambda x: from_dict(lambda x: to_class(RemoteFieldValue, x), x), from_none], self.field)
        return result


@dataclass
class RemoteRelationshipDef:
    """Definition object"""
    """Column(s) in the table that is used for joining with remote schema field.
    All join keys in remote_field must appear here.
    """
    hasura_fields: List[str]
    """The schema tree ending at the field in remote schema which needs to be joined with."""
    remote_field: Dict[str, RemoteFieldValue]
    """Name of the remote schema to join with"""
    remote_schema: str

    @staticmethod
    def from_dict(obj: Any) -> 'RemoteRelationshipDef':
        assert isinstance(obj, dict)
        hasura_fields = from_list(from_str, obj.get("hasura_fields"))
        remote_field = from_dict(RemoteFieldValue.from_dict, obj.get("remote_field"))
        remote_schema = from_str(obj.get("remote_schema"))
        return RemoteRelationshipDef(hasura_fields, remote_field, remote_schema)

    def to_dict(self) -> dict:
        result: dict = {}
        result["hasura_fields"] = from_list(from_str, self.hasura_fields)
        result["remote_field"] = from_dict(lambda x: to_class(RemoteFieldValue, x), self.remote_field)
        result["remote_schema"] = from_str(self.remote_schema)
        return result


@dataclass
class RemoteRelationship:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/remote-relationships.html#args-syntax
    """
    """Definition object"""
    definition: RemoteRelationshipDef
    """Name of the remote relationship"""
    name: str

    @staticmethod
    def from_dict(obj: Any) -> 'RemoteRelationship':
        assert isinstance(obj, dict)
        definition = RemoteRelationshipDef.from_dict(obj.get("definition"))
        name = from_str(obj.get("name"))
        return RemoteRelationship(definition, name)

    def to_dict(self) -> dict:
        result: dict = {}
        result["definition"] = to_class(RemoteRelationshipDef, self.definition)
        result["name"] = from_str(self.name)
        return result


@dataclass
class SelectPermission:
    """The permission definition
    
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#selectpermission
    """
    """Only these columns are selectable (or all when '*' is specified)"""
    columns: Union[List[str], EventTriggerColumnsEnum]
    """Toggle allowing aggregate queries"""
    allow_aggregations: Optional[bool] = None
    """Only these computed fields are selectable"""
    computed_fields: Optional[List[str]] = None
    """Only the rows where this precondition holds true are selectable"""
    filter: Optional[Dict[str, Union[float, Dict[str, Any], str]]] = None
    """The maximum number of rows that can be returned"""
    limit: Optional[int] = None

    @staticmethod
    def from_dict(obj: Any) -> 'SelectPermission':
        assert isinstance(obj, dict)
        columns = from_union([lambda x: from_list(from_str, x), EventTriggerColumnsEnum], obj.get("columns"))
        allow_aggregations = from_union([from_bool, from_none], obj.get("allow_aggregations"))
        computed_fields = from_union([lambda x: from_list(from_str, x), from_none], obj.get("computed_fields"))
        filter = from_union([lambda x: from_dict(lambda x: from_union([lambda x: from_dict(lambda x: x, x), from_float, from_str], x), x), from_none], obj.get("filter"))
        limit = from_union([from_int, from_none], obj.get("limit"))
        return SelectPermission(columns, allow_aggregations, computed_fields, filter, limit)

    def to_dict(self) -> dict:
        result: dict = {}
        result["columns"] = from_union([lambda x: from_list(from_str, x), lambda x: to_enum(EventTriggerColumnsEnum, x)], self.columns)
        result["allow_aggregations"] = from_union([from_bool, from_none], self.allow_aggregations)
        result["computed_fields"] = from_union([lambda x: from_list(from_str, x), from_none], self.computed_fields)
        result["filter"] = from_union([lambda x: from_dict(lambda x: from_union([lambda x: from_dict(lambda x: x, x), to_float, from_str], x), x), from_none], self.filter)
        result["limit"] = from_union([from_int, from_none], self.limit)
        return result


@dataclass
class SelectPermissionEntry:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#create-select-permission-syntax
    """
    """The permission definition"""
    permission: SelectPermission
    """Role"""
    role: str
    """Comment"""
    comment: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'SelectPermissionEntry':
        assert isinstance(obj, dict)
        permission = SelectPermission.from_dict(obj.get("permission"))
        role = from_str(obj.get("role"))
        comment = from_union([from_str, from_none], obj.get("comment"))
        return SelectPermissionEntry(permission, role, comment)

    def to_dict(self) -> dict:
        result: dict = {}
        result["permission"] = to_class(SelectPermission, self.permission)
        result["role"] = from_str(self.role)
        result["comment"] = from_union([from_str, from_none], self.comment)
        return result


@dataclass
class UpdatePermission:
    """The permission definition
    
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#updatepermission
    """
    """Only these columns are selectable (or all when '*' is specified)"""
    columns: Union[List[str], EventTriggerColumnsEnum]
    """Postcondition which must be satisfied by rows which have been updated"""
    check: Optional[Dict[str, Union[float, Dict[str, Any], str]]] = None
    """Only the rows where this precondition holds true are updatable"""
    filter: Optional[Dict[str, Union[float, Dict[str, Any], str]]] = None
    """Preset values for columns that can be sourced from session variables or static values"""
    set: Optional[Dict[str, str]] = None

    @staticmethod
    def from_dict(obj: Any) -> 'UpdatePermission':
        assert isinstance(obj, dict)
        columns = from_union([lambda x: from_list(from_str, x), EventTriggerColumnsEnum], obj.get("columns"))
        check = from_union([lambda x: from_dict(lambda x: from_union([lambda x: from_dict(lambda x: x, x), from_float, from_str], x), x), from_none], obj.get("check"))
        filter = from_union([lambda x: from_dict(lambda x: from_union([lambda x: from_dict(lambda x: x, x), from_float, from_str], x), x), from_none], obj.get("filter"))
        set = from_union([lambda x: from_dict(from_str, x), from_none], obj.get("set"))
        return UpdatePermission(columns, check, filter, set)

    def to_dict(self) -> dict:
        result: dict = {}
        result["columns"] = from_union([lambda x: from_list(from_str, x), lambda x: to_enum(EventTriggerColumnsEnum, x)], self.columns)
        result["check"] = from_union([lambda x: from_dict(lambda x: from_union([lambda x: from_dict(lambda x: x, x), to_float, from_str], x), x), from_none], self.check)
        result["filter"] = from_union([lambda x: from_dict(lambda x: from_union([lambda x: from_dict(lambda x: x, x), to_float, from_str], x), x), from_none], self.filter)
        result["set"] = from_union([lambda x: from_dict(from_str, x), from_none], self.set)
        return result


@dataclass
class UpdatePermissionEntry:
    """
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/permission.html#create-update-permission-syntax
    """
    """The permission definition"""
    permission: UpdatePermission
    """Role"""
    role: str
    """Comment"""
    comment: Optional[str] = None

    @staticmethod
    def from_dict(obj: Any) -> 'UpdatePermissionEntry':
        assert isinstance(obj, dict)
        permission = UpdatePermission.from_dict(obj.get("permission"))
        role = from_str(obj.get("role"))
        comment = from_union([from_str, from_none], obj.get("comment"))
        return UpdatePermissionEntry(permission, role, comment)

    def to_dict(self) -> dict:
        result: dict = {}
        result["permission"] = to_class(UpdatePermission, self.permission)
        result["role"] = from_str(self.role)
        result["comment"] = from_union([from_str, from_none], self.comment)
        return result


@dataclass
class TableEntry:
    """Representation of a table in metadata, 'tables.yaml' and 'metadata.json'"""
    table: QualifiedTable
    array_relationships: Optional[List[ArrayRelationship]] = None
    computed_fields: Optional[List[ComputedField]] = None
    """Configuration for the table/view
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/table-view.html#table-config
    """
    configuration: Optional[TableConfig] = None
    delete_permissions: Optional[List[DeletePermissionEntry]] = None
    event_triggers: Optional[List[EventTrigger]] = None
    insert_permissions: Optional[List[InsertPermissionEntry]] = None
    is_enum: Optional[bool] = None
    object_relationships: Optional[List[ObjectRelationship]] = None
    remote_relationships: Optional[List[RemoteRelationship]] = None
    select_permissions: Optional[List[SelectPermissionEntry]] = None
    update_permissions: Optional[List[UpdatePermissionEntry]] = None

    @staticmethod
    def from_dict(obj: Any) -> 'TableEntry':
        assert isinstance(obj, dict)
        table = QualifiedTable.from_dict(obj.get("table"))
        array_relationships = from_union([lambda x: from_list(ArrayRelationship.from_dict, x), from_none], obj.get("array_relationships"))
        computed_fields = from_union([lambda x: from_list(ComputedField.from_dict, x), from_none], obj.get("computed_fields"))
        configuration = from_union([TableConfig.from_dict, from_none], obj.get("configuration"))
        delete_permissions = from_union([lambda x: from_list(DeletePermissionEntry.from_dict, x), from_none], obj.get("delete_permissions"))
        event_triggers = from_union([lambda x: from_list(EventTrigger.from_dict, x), from_none], obj.get("event_triggers"))
        insert_permissions = from_union([lambda x: from_list(InsertPermissionEntry.from_dict, x), from_none], obj.get("insert_permissions"))
        is_enum = from_union([from_bool, from_none], obj.get("is_enum"))
        object_relationships = from_union([lambda x: from_list(ObjectRelationship.from_dict, x), from_none], obj.get("object_relationships"))
        remote_relationships = from_union([lambda x: from_list(RemoteRelationship.from_dict, x), from_none], obj.get("remote_relationships"))
        select_permissions = from_union([lambda x: from_list(SelectPermissionEntry.from_dict, x), from_none], obj.get("select_permissions"))
        update_permissions = from_union([lambda x: from_list(UpdatePermissionEntry.from_dict, x), from_none], obj.get("update_permissions"))
        return TableEntry(table, array_relationships, computed_fields, configuration, delete_permissions, event_triggers, insert_permissions, is_enum, object_relationships, remote_relationships, select_permissions, update_permissions)

    def to_dict(self) -> dict:
        result: dict = {}
        result["table"] = to_class(QualifiedTable, self.table)
        result["array_relationships"] = from_union([lambda x: from_list(lambda x: to_class(ArrayRelationship, x), x), from_none], self.array_relationships)
        result["computed_fields"] = from_union([lambda x: from_list(lambda x: to_class(ComputedField, x), x), from_none], self.computed_fields)
        result["configuration"] = from_union([lambda x: to_class(TableConfig, x), from_none], self.configuration)
        result["delete_permissions"] = from_union([lambda x: from_list(lambda x: to_class(DeletePermissionEntry, x), x), from_none], self.delete_permissions)
        result["event_triggers"] = from_union([lambda x: from_list(lambda x: to_class(EventTrigger, x), x), from_none], self.event_triggers)
        result["insert_permissions"] = from_union([lambda x: from_list(lambda x: to_class(InsertPermissionEntry, x), x), from_none], self.insert_permissions)
        result["is_enum"] = from_union([from_bool, from_none], self.is_enum)
        result["object_relationships"] = from_union([lambda x: from_list(lambda x: to_class(ObjectRelationship, x), x), from_none], self.object_relationships)
        result["remote_relationships"] = from_union([lambda x: from_list(lambda x: to_class(RemoteRelationship, x), x), from_none], self.remote_relationships)
        result["select_permissions"] = from_union([lambda x: from_list(lambda x: to_class(SelectPermissionEntry, x), x), from_none], self.select_permissions)
        result["update_permissions"] = from_union([lambda x: from_list(lambda x: to_class(UpdatePermissionEntry, x), x), from_none], self.update_permissions)
        return result


@dataclass
class HasuraMetadataV2:
    """Type used in exported 'metadata.json' and replace metadata endpoint
    
    https://hasura.io/docs/1.0/graphql/manual/api-reference/schema-metadata-api/manage-metadata.html#replace-metadata
    """
    tables: List[TableEntry]
    version: float
    actions: Optional[List[Action]] = None
    allowlist: Optional[List[AllowList]] = None
    cron_triggers: Optional[List[CronTrigger]] = None
    custom_types: Optional[CustomTypes] = None
    functions: Optional[List[CustomFunction]] = None
    query_collections: Optional[List[QueryCollectionEntry]] = None
    remote_schemas: Optional[List[RemoteSchema]] = None

    @staticmethod
    def from_dict(obj: Any) -> 'HasuraMetadataV2':
        assert isinstance(obj, dict)
        tables = from_list(TableEntry.from_dict, obj.get("tables"))
        version = from_float(obj.get("version"))
        actions = from_union([lambda x: from_list(Action.from_dict, x), from_none], obj.get("actions"))
        allowlist = from_union([lambda x: from_list(AllowList.from_dict, x), from_none], obj.get("allowlist"))
        cron_triggers = from_union([lambda x: from_list(CronTrigger.from_dict, x), from_none], obj.get("cron_triggers"))
        custom_types = from_union([CustomTypes.from_dict, from_none], obj.get("custom_types"))
        functions = from_union([lambda x: from_list(CustomFunction.from_dict, x), from_none], obj.get("functions"))
        query_collections = from_union([lambda x: from_list(QueryCollectionEntry.from_dict, x), from_none], obj.get("query_collections"))
        remote_schemas = from_union([lambda x: from_list(RemoteSchema.from_dict, x), from_none], obj.get("remote_schemas"))
        return HasuraMetadataV2(tables, version, actions, allowlist, cron_triggers, custom_types, functions, query_collections, remote_schemas)

    def to_dict(self) -> dict:
        result: dict = {}
        result["tables"] = from_list(lambda x: to_class(TableEntry, x), self.tables)
        result["version"] = to_float(self.version)
        result["actions"] = from_union([lambda x: from_list(lambda x: to_class(Action, x), x), from_none], self.actions)
        result["allowlist"] = from_union([lambda x: from_list(lambda x: to_class(AllowList, x), x), from_none], self.allowlist)
        result["cron_triggers"] = from_union([lambda x: from_list(lambda x: to_class(CronTrigger, x), x), from_none], self.cron_triggers)
        result["custom_types"] = from_union([lambda x: to_class(CustomTypes, x), from_none], self.custom_types)
        result["functions"] = from_union([lambda x: from_list(lambda x: to_class(CustomFunction, x), x), from_none], self.functions)
        result["query_collections"] = from_union([lambda x: from_list(lambda x: to_class(QueryCollectionEntry, x), x), from_none], self.query_collections)
        result["remote_schemas"] = from_union([lambda x: from_list(lambda x: to_class(RemoteSchema, x), x), from_none], self.remote_schemas)
        return result


def pg_column_from_dict(s: Any) -> str:
    return from_str(s)


def pg_column_to_dict(x: str) -> Any:
    return from_str(x)


def computed_field_name_from_dict(s: Any) -> str:
    return from_str(s)


def computed_field_name_to_dict(x: str) -> Any:
    return from_str(x)


def role_name_from_dict(s: Any) -> str:
    return from_str(s)


def role_name_to_dict(x: str) -> Any:
    return from_str(x)


def trigger_name_from_dict(s: Any) -> str:
    return from_str(s)


def trigger_name_to_dict(x: str) -> Any:
    return from_str(x)


def remote_relationship_name_from_dict(s: Any) -> str:
    return from_str(s)


def remote_relationship_name_to_dict(x: str) -> Any:
    return from_str(x)


def remote_schema_name_from_dict(s: Any) -> str:
    return from_str(s)


def remote_schema_name_to_dict(x: str) -> Any:
    return from_str(x)


def collection_name_from_dict(s: Any) -> str:
    return from_str(s)


def collection_name_to_dict(x: str) -> Any:
    return from_str(x)


def graph_ql_name_from_dict(s: Any) -> str:
    return from_str(s)


def graph_ql_name_to_dict(x: str) -> Any:
    return from_str(x)


def graph_ql_type_from_dict(s: Any) -> str:
    return from_str(s)


def graph_ql_type_to_dict(x: str) -> Any:
    return from_str(x)


def relationship_name_from_dict(s: Any) -> str:
    return from_str(s)


def relationship_name_to_dict(x: str) -> Any:
    return from_str(x)


def action_name_from_dict(s: Any) -> str:
    return from_str(s)


def action_name_to_dict(x: str) -> Any:
    return from_str(x)


def webhook_url_from_dict(s: Any) -> str:
    return from_str(s)


def webhook_url_to_dict(x: str) -> Any:
    return from_str(x)


def table_name_from_dict(s: Any) -> Union[QualifiedTable, str]:
    return from_union([QualifiedTable.from_dict, from_str], s)


def table_name_to_dict(x: Union[QualifiedTable, str]) -> Any:
    return from_union([lambda x: to_class(QualifiedTable, x), from_str], x)


def qualified_table_from_dict(s: Any) -> QualifiedTable:
    return QualifiedTable.from_dict(s)


def qualified_table_to_dict(x: QualifiedTable) -> Any:
    return to_class(QualifiedTable, x)


def table_config_from_dict(s: Any) -> TableConfig:
    return TableConfig.from_dict(s)


def table_config_to_dict(x: TableConfig) -> Any:
    return to_class(TableConfig, x)


def table_entry_from_dict(s: Any) -> TableEntry:
    return TableEntry.from_dict(s)


def table_entry_to_dict(x: TableEntry) -> Any:
    return to_class(TableEntry, x)


def custom_root_fields_from_dict(s: Any) -> CustomRootFields:
    return CustomRootFields.from_dict(s)


def custom_root_fields_to_dict(x: CustomRootFields) -> Any:
    return to_class(CustomRootFields, x)


def custom_column_names_from_dict(s: Any) -> Dict[str, str]:
    return from_dict(from_str, s)


def custom_column_names_to_dict(x: Dict[str, str]) -> Any:
    return from_dict(from_str, x)


def function_name_from_dict(s: Any) -> Union[QualifiedFunction, str]:
    return from_union([QualifiedFunction.from_dict, from_str], s)


def function_name_to_dict(x: Union[QualifiedFunction, str]) -> Any:
    return from_union([lambda x: to_class(QualifiedFunction, x), from_str], x)


def qualified_function_from_dict(s: Any) -> QualifiedFunction:
    return QualifiedFunction.from_dict(s)


def qualified_function_to_dict(x: QualifiedFunction) -> Any:
    return to_class(QualifiedFunction, x)


def custom_function_from_dict(s: Any) -> CustomFunction:
    return CustomFunction.from_dict(s)


def custom_function_to_dict(x: CustomFunction) -> Any:
    return to_class(CustomFunction, x)


def function_configuration_from_dict(s: Any) -> FunctionConfiguration:
    return FunctionConfiguration.from_dict(s)


def function_configuration_to_dict(x: FunctionConfiguration) -> Any:
    return to_class(FunctionConfiguration, x)


def object_relationship_from_dict(s: Any) -> ObjectRelationship:
    return ObjectRelationship.from_dict(s)


def object_relationship_to_dict(x: ObjectRelationship) -> Any:
    return to_class(ObjectRelationship, x)


def obj_rel_using_from_dict(s: Any) -> ObjRelUsing:
    return ObjRelUsing.from_dict(s)


def obj_rel_using_to_dict(x: ObjRelUsing) -> Any:
    return to_class(ObjRelUsing, x)


def obj_rel_using_manual_mapping_from_dict(s: Any) -> ObjRelUsingManualMapping:
    return ObjRelUsingManualMapping.from_dict(s)


def obj_rel_using_manual_mapping_to_dict(x: ObjRelUsingManualMapping) -> Any:
    return to_class(ObjRelUsingManualMapping, x)


def array_relationship_from_dict(s: Any) -> ArrayRelationship:
    return ArrayRelationship.from_dict(s)


def array_relationship_to_dict(x: ArrayRelationship) -> Any:
    return to_class(ArrayRelationship, x)


def arr_rel_using_from_dict(s: Any) -> ArrRelUsing:
    return ArrRelUsing.from_dict(s)


def arr_rel_using_to_dict(x: ArrRelUsing) -> Any:
    return to_class(ArrRelUsing, x)


def arr_rel_using_f_key_on_from_dict(s: Any) -> ArrRelUsingFKeyOn:
    return ArrRelUsingFKeyOn.from_dict(s)


def arr_rel_using_f_key_on_to_dict(x: ArrRelUsingFKeyOn) -> Any:
    return to_class(ArrRelUsingFKeyOn, x)


def arr_rel_using_manual_mapping_from_dict(s: Any) -> ArrRelUsingManualMapping:
    return ArrRelUsingManualMapping.from_dict(s)


def arr_rel_using_manual_mapping_to_dict(x: ArrRelUsingManualMapping) -> Any:
    return to_class(ArrRelUsingManualMapping, x)


def column_presets_expression_from_dict(s: Any) -> Dict[str, str]:
    return from_dict(from_str, s)


def column_presets_expression_to_dict(x: Dict[str, str]) -> Any:
    return from_dict(from_str, x)


def insert_permission_entry_from_dict(s: Any) -> InsertPermissionEntry:
    return InsertPermissionEntry.from_dict(s)


def insert_permission_entry_to_dict(x: InsertPermissionEntry) -> Any:
    return to_class(InsertPermissionEntry, x)


def insert_permission_from_dict(s: Any) -> InsertPermission:
    return InsertPermission.from_dict(s)


def insert_permission_to_dict(x: InsertPermission) -> Any:
    return to_class(InsertPermission, x)


def select_permission_entry_from_dict(s: Any) -> SelectPermissionEntry:
    return SelectPermissionEntry.from_dict(s)


def select_permission_entry_to_dict(x: SelectPermissionEntry) -> Any:
    return to_class(SelectPermissionEntry, x)


def select_permission_from_dict(s: Any) -> SelectPermission:
    return SelectPermission.from_dict(s)


def select_permission_to_dict(x: SelectPermission) -> Any:
    return to_class(SelectPermission, x)


def update_permission_entry_from_dict(s: Any) -> UpdatePermissionEntry:
    return UpdatePermissionEntry.from_dict(s)


def update_permission_entry_to_dict(x: UpdatePermissionEntry) -> Any:
    return to_class(UpdatePermissionEntry, x)


def update_permission_from_dict(s: Any) -> UpdatePermission:
    return UpdatePermission.from_dict(s)


def update_permission_to_dict(x: UpdatePermission) -> Any:
    return to_class(UpdatePermission, x)


def delete_permission_entry_from_dict(s: Any) -> DeletePermissionEntry:
    return DeletePermissionEntry.from_dict(s)


def delete_permission_entry_to_dict(x: DeletePermissionEntry) -> Any:
    return to_class(DeletePermissionEntry, x)


def delete_permission_from_dict(s: Any) -> DeletePermission:
    return DeletePermission.from_dict(s)


def delete_permission_to_dict(x: DeletePermission) -> Any:
    return to_class(DeletePermission, x)


def computed_field_from_dict(s: Any) -> ComputedField:
    return ComputedField.from_dict(s)


def computed_field_to_dict(x: ComputedField) -> Any:
    return to_class(ComputedField, x)


def computed_field_definition_from_dict(s: Any) -> ComputedFieldDefinition:
    return ComputedFieldDefinition.from_dict(s)


def computed_field_definition_to_dict(x: ComputedFieldDefinition) -> Any:
    return to_class(ComputedFieldDefinition, x)


def event_trigger_from_dict(s: Any) -> EventTrigger:
    return EventTrigger.from_dict(s)


def event_trigger_to_dict(x: EventTrigger) -> Any:
    return to_class(EventTrigger, x)


def event_trigger_definition_from_dict(s: Any) -> EventTriggerDefinition:
    return EventTriggerDefinition.from_dict(s)


def event_trigger_definition_to_dict(x: EventTriggerDefinition) -> Any:
    return to_class(EventTriggerDefinition, x)


def event_trigger_columns_from_dict(s: Any) -> Union[List[str], EventTriggerColumnsEnum]:
    return from_union([lambda x: from_list(from_str, x), EventTriggerColumnsEnum], s)


def event_trigger_columns_to_dict(x: Union[List[str], EventTriggerColumnsEnum]) -> Any:
    return from_union([lambda x: from_list(from_str, x), lambda x: to_enum(EventTriggerColumnsEnum, x)], x)


def operation_spec_from_dict(s: Any) -> OperationSpec:
    return OperationSpec.from_dict(s)


def operation_spec_to_dict(x: OperationSpec) -> Any:
    return to_class(OperationSpec, x)


def header_from_value_from_dict(s: Any) -> HeaderFromValue:
    return HeaderFromValue.from_dict(s)


def header_from_value_to_dict(x: HeaderFromValue) -> Any:
    return to_class(HeaderFromValue, x)


def header_from_env_from_dict(s: Any) -> HeaderFromEnv:
    return HeaderFromEnv.from_dict(s)


def header_from_env_to_dict(x: HeaderFromEnv) -> Any:
    return to_class(HeaderFromEnv, x)


def retry_conf_from_dict(s: Any) -> RetryConf:
    return RetryConf.from_dict(s)


def retry_conf_to_dict(x: RetryConf) -> Any:
    return to_class(RetryConf, x)


def cron_trigger_from_dict(s: Any) -> CronTrigger:
    return CronTrigger.from_dict(s)


def cron_trigger_to_dict(x: CronTrigger) -> Any:
    return to_class(CronTrigger, x)


def retry_conf_st_from_dict(s: Any) -> RetryConfST:
    return RetryConfST.from_dict(s)


def retry_conf_st_to_dict(x: RetryConfST) -> Any:
    return to_class(RetryConfST, x)


def remote_schema_from_dict(s: Any) -> RemoteSchema:
    return RemoteSchema.from_dict(s)


def remote_schema_to_dict(x: RemoteSchema) -> Any:
    return to_class(RemoteSchema, x)


def remote_schema_def_from_dict(s: Any) -> RemoteSchemaDef:
    return RemoteSchemaDef.from_dict(s)


def remote_schema_def_to_dict(x: RemoteSchemaDef) -> Any:
    return to_class(RemoteSchemaDef, x)


def remote_relationship_from_dict(s: Any) -> RemoteRelationship:
    return RemoteRelationship.from_dict(s)


def remote_relationship_to_dict(x: RemoteRelationship) -> Any:
    return to_class(RemoteRelationship, x)


def remote_relationship_def_from_dict(s: Any) -> RemoteRelationshipDef:
    return RemoteRelationshipDef.from_dict(s)


def remote_relationship_def_to_dict(x: RemoteRelationshipDef) -> Any:
    return to_class(RemoteRelationshipDef, x)


def remote_field_from_dict(s: Any) -> Dict[str, RemoteFieldValue]:
    return from_dict(RemoteFieldValue.from_dict, s)


def remote_field_to_dict(x: Dict[str, RemoteFieldValue]) -> Any:
    return from_dict(lambda x: to_class(RemoteFieldValue, x), x)


def input_arguments_from_dict(s: Any) -> Dict[str, str]:
    return from_dict(from_str, s)


def input_arguments_to_dict(x: Dict[str, str]) -> Any:
    return from_dict(from_str, x)


def query_collection_entry_from_dict(s: Any) -> QueryCollectionEntry:
    return QueryCollectionEntry.from_dict(s)


def query_collection_entry_to_dict(x: QueryCollectionEntry) -> Any:
    return to_class(QueryCollectionEntry, x)


def query_collection_from_dict(s: Any) -> QueryCollection:
    return QueryCollection.from_dict(s)


def query_collection_to_dict(x: QueryCollection) -> Any:
    return to_class(QueryCollection, x)


def allow_list_from_dict(s: Any) -> AllowList:
    return AllowList.from_dict(s)


def allow_list_to_dict(x: AllowList) -> Any:
    return to_class(AllowList, x)


def custom_types_from_dict(s: Any) -> CustomTypes:
    return CustomTypes.from_dict(s)


def custom_types_to_dict(x: CustomTypes) -> Any:
    return to_class(CustomTypes, x)


def input_object_type_from_dict(s: Any) -> InputObjectType:
    return InputObjectType.from_dict(s)


def input_object_type_to_dict(x: InputObjectType) -> Any:
    return to_class(InputObjectType, x)


def input_object_field_from_dict(s: Any) -> InputObjectField:
    return InputObjectField.from_dict(s)


def input_object_field_to_dict(x: InputObjectField) -> Any:
    return to_class(InputObjectField, x)


def object_type_from_dict(s: Any) -> ObjectType:
    return ObjectType.from_dict(s)


def object_type_to_dict(x: ObjectType) -> Any:
    return to_class(ObjectType, x)


def object_field_from_dict(s: Any) -> ObjectField:
    return ObjectField.from_dict(s)


def object_field_to_dict(x: ObjectField) -> Any:
    return to_class(ObjectField, x)


def custom_type_object_relationship_from_dict(s: Any) -> CustomTypeObjectRelationship:
    return CustomTypeObjectRelationship.from_dict(s)


def custom_type_object_relationship_to_dict(x: CustomTypeObjectRelationship) -> Any:
    return to_class(CustomTypeObjectRelationship, x)


def scalar_type_from_dict(s: Any) -> ScalarType:
    return ScalarType.from_dict(s)


def scalar_type_to_dict(x: ScalarType) -> Any:
    return to_class(ScalarType, x)


def enum_type_from_dict(s: Any) -> EnumType:
    return EnumType.from_dict(s)


def enum_type_to_dict(x: EnumType) -> Any:
    return to_class(EnumType, x)


def enum_value_from_dict(s: Any) -> EnumValue:
    return EnumValue.from_dict(s)


def enum_value_to_dict(x: EnumValue) -> Any:
    return to_class(EnumValue, x)


def action_from_dict(s: Any) -> Action:
    return Action.from_dict(s)


def action_to_dict(x: Action) -> Any:
    return to_class(Action, x)


def action_definition_from_dict(s: Any) -> ActionDefinition:
    return ActionDefinition.from_dict(s)


def action_definition_to_dict(x: ActionDefinition) -> Any:
    return to_class(ActionDefinition, x)


def input_argument_from_dict(s: Any) -> InputArgument:
    return InputArgument.from_dict(s)


def input_argument_to_dict(x: InputArgument) -> Any:
    return to_class(InputArgument, x)


def hasura_metadata_v2_from_dict(s: Any) -> HasuraMetadataV2:
    return HasuraMetadataV2.from_dict(s)


def hasura_metadata_v2_to_dict(x: HasuraMetadataV2) -> Any:
    return to_class(HasuraMetadataV2, x)
