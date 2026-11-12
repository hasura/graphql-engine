"""Action scalar metadata must survive source naming-convention switches.

These tests use an isolated source and the existing local mirror webhook. No
customer database or customer webhook is involved.
"""
import copy
import json
import threading
from http.server import BaseHTTPRequestHandler, HTTPServer

from graphql import (
    GraphQLArgument, GraphQLField, GraphQLInputObjectField, GraphQLInputObjectType,
    GraphQLList, GraphQLNonNull, GraphQLObjectType, GraphQLScalarType, GraphQLSchema,
    build_client_schema, graphql, introspection_query,
)

import pytest

SCALARS = ("jsonb", "bigint", "timestamp", "timestamptz")
VALUES = ({"nested": [1, True]}, 42, "2026-01-02T03:04:05", "2026-01-02T03:04:05Z")


def scalar_names(spelling):
    if spelling == "mixed":
        return [name if i % 2 == 0 else name.title() for i, name in enumerate(SCALARS)]
    return [name if spelling == "lower" else name.title() for name in SCALARS]


def action(name, argument_type, output_type, handler):
    return {
        "name": name,
        "definition": {
            "kind": "synchronous",
            "type": "mutation",
            "arguments": [{"name": "arg", "type": argument_type}],
            "output_type": output_type,
            "handler": handler,
        },
    }


def assert_valid_schema(hge_ctx):
    # A targeted __type lookup cannot detect dangling references elsewhere.
    response = hge_ctx.v1graphqlq({"query": introspection_query})
    assert "errors" not in response, response
    schema = response["data"]["__schema"]
    types = {type_["name"]: type_ for type_ in schema["types"]}

    def check_reference(reference):
        assert reference is not None, "Missing type reference"
        if reference["kind"] in ("NON_NULL", "LIST"):
            assert reference.get("name") is None, reference
            check_reference(reference.get("ofType"))
        else:
            name = reference.get("name")
            assert name in types, f"Dangling type reference: {reference}"
            assert reference["kind"] == types[name]["kind"], reference

    for operation in ("queryType", "mutationType", "subscriptionType"):
        if schema.get(operation):
            assert schema[operation]["name"] in types, schema[operation]
    for type_ in types.values():
        for field in (type_.get("fields") or []) + (type_.get("inputFields") or []):
            check_reference(field["type"])
            for argument in field.get("args") or []:
                check_reference(argument["type"])
        for reference in (type_.get("interfaces") or []) + (type_.get("possibleTypes") or []):
            check_reference(reference)
    for directive in schema["directives"]:
        for argument in directive["args"]:
            check_reference(argument["type"])

    # Also exercise a real GraphQL client's reconstruction of the whole schema,
    # including input/output kinds, abstract types, and argument defaults.
    build_client_schema(response["data"])
    return schema


def replace(hge_ctx, metadata):
    result = hge_ctx.v1metadataq({
        "type": "replace_metadata",
        "version": 2,
        "args": {"metadata": metadata, "allow_inconsistent_metadata": False},
    })
    inconsistent = hge_ctx.v1metadataq({"type": "get_inconsistent_metadata", "args": {}})
    assert inconsistent["is_consistent"], inconsistent
    assert not inconsistent["inconsistent_objects"], inconsistent
    assert_valid_schema(hge_ctx)
    return result


def set_convention(metadata, convention):
    source = next(source for source in metadata["sources"] if source["name"] == "pg1")
    source["customization"] = {"naming_convention": convention}


@pytest.fixture(scope="class")
def scalar_remote_schema():
    # A real local remote GraphQL service, not a mocked introspection response.
    # Keep its lowercase scalar names independent of the PG source convention.
    scalars = [GraphQLScalarType(
        name=name, serialize=lambda value: value, parse_value=lambda value: value,
        parse_literal=lambda node: getattr(node, "value", None),
    ) for name in SCALARS]
    input_type = GraphQLInputObjectType("RemoteAliasInput", {
        f"field{i}": GraphQLInputObjectField(GraphQLNonNull(GraphQLList(GraphQLNonNull(scalar))))
        for i, scalar in enumerate(scalars)
    })
    output_type = GraphQLObjectType("RemoteAliasOutput", {
        f"field{i}": GraphQLField(GraphQLNonNull(GraphQLList(GraphQLNonNull(scalar))))
        for i, scalar in enumerate(scalars)
    })
    schema = GraphQLSchema(query=GraphQLObjectType("RemoteAliasQuery", {
        "remote_scalar_echo": GraphQLField(
            output_type, args={"arg": GraphQLArgument(GraphQLNonNull(input_type))},
            resolver=lambda root, info, arg: arg,
        ),
    }))

    class Handler(BaseHTTPRequestHandler):
        def do_POST(self):
            payload = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
            result = graphql(
                schema, payload["query"], variable_values=payload.get("variables"),
                operation_name=payload.get("operationName"),
            )
            body = json.dumps(result.to_dict()).encode("utf-8")
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def log_message(self, format, *args):
            pass

    server = HTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=server.serve_forever)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_port}"
    finally:
        server.shutdown()
        server.server_close()
        thread.join()


class ActionScalarAliases:
    @pytest.fixture(scope="class", autouse=True)
    def scalar_source(self, add_source):
        backend = add_source("pg1", customization={"naming_convention": "hasura-default"})
        with backend.engine.connect() as connection:
            connection.execute("""
                CREATE TABLE public.alias_scalar_rows (
                    id integer PRIMARY KEY,
                    payload jsonb,
                    count bigint,
                    created timestamp,
                    expires timestamptz
                )
            """)
            connection.execute("""
                INSERT INTO public.alias_scalar_rows VALUES
                (1, '{"nested": [1, true]}', 42,
                 '2026-01-02T03:04:05', '2026-01-02T03:04:05Z')
            """)
        try:
            yield backend
        finally:
            with backend.engine.connect() as connection:
                connection.execute("DROP TABLE public.alias_scalar_rows")

    @pytest.fixture(autouse=True)
    def original_metadata(self, hge_ctx):
        original = hge_ctx.v1metadataq({"type": "export_metadata", "args": {}})
        yield copy.deepcopy(original)
        replace(hge_ctx, original)

    @pytest.fixture
    def handler(self, actions_fixture):
        return actions_fixture.url + "/mirror-action"

    @pytest.mark.parametrize("convention", ["hasura-default", "graphql-default"])
    @pytest.mark.parametrize("spelling", ["lower", "title", "mixed"])
    def test_nested_types_survive_round_trip(self, hge_ctx, original_metadata, handler, convention, spelling):
        metadata = original_metadata
        names = scalar_names(spelling)
        metadata["custom_types"] = {
            "input_objects": [{
                "name": "AliasInput",
                "fields": [{"name": f"field{i}", "type": f"[{name}!]!"} for i, name in enumerate(names)],
            }],
            "objects": [{
                "name": "AliasOutput",
                "fields": [{"name": f"field{i}", "type": f"[{name}!]!"} for i, name in enumerate(names)],
            }],
        }
        metadata["actions"] = [action("alias_echo", "AliasInput!", "AliasOutput!", handler)]
        custom_types = copy.deepcopy(metadata["custom_types"])
        arguments = {f"field{i}": [value] for i, value in enumerate(VALUES)}
        opposite = "graphql-default" if convention == "hasura-default" else "hasura-default"
        for current in (convention, opposite, convention):
            set_convention(metadata, current)
            replace(hge_ctx, metadata)
            inconsistent = hge_ctx.v1metadataq({"type": "get_inconsistent_metadata", "args": {}})
            assert inconsistent["is_consistent"], inconsistent
            response = hge_ctx.v1graphqlq({
                "query": "mutation($arg: AliasInput!) { alias_echo(arg: $arg) { field0 field1 field2 field3 } }",
                "variables": {"arg": arguments},
            })
            assert response == {"data": {"alias_echo": arguments}}
            invalid = copy.deepcopy(arguments)
            invalid["field1"] = [True]
            response = hge_ctx.v1graphqlq({
                "query": "mutation($arg: AliasInput!) { alias_echo(arg: $arg) { field0 field1 field2 field3 } }",
                "variables": {"arg": invalid},
            })
            assert response["errors"][0]["extensions"]["code"] == "parse-failed", response
            # A spelling is a real GraphQL type name, not a rewrite to the
            # source's active convention.
            result = hge_ctx.v1graphqlq({"query": """
                { __type(name: "AliasInput") {
                    inputFields { name type { kind ofType { kind ofType { kind ofType { name } } } } }
                } }
            """})
            actual = {field["name"]: field["type"]["ofType"]["ofType"]["ofType"]["name"]
                      for field in result["data"]["__type"]["inputFields"]}
            assert actual == {f"field{i}": name for i, name in enumerate(names)}
            exported = hge_ctx.v1metadataq({"type": "export_metadata", "args": {}})
            assert exported["custom_types"]["input_objects"] == custom_types["input_objects"]
            assert exported["custom_types"]["objects"] == custom_types["objects"]
            assert not exported["custom_types"].get("scalars")

    @pytest.mark.parametrize("convention", ["hasura-default", "graphql-default"])
    @pytest.mark.parametrize("spelling", ["lower", "title"])
    def test_direct_scalar_arguments_and_outputs(self, hge_ctx, original_metadata, handler, convention, spelling):
        metadata = original_metadata
        names = scalar_names(spelling)
        metadata["custom_types"] = {}
        metadata["actions"] = [
            action(f"alias_direct_{i}", f"{name}!", f"{name}!", handler)
            for i, name in enumerate(names)
        ]
        set_convention(metadata, convention)
        replace(hge_ctx, metadata)
        for i, (name, value) in enumerate(zip(names, VALUES)):
            response = hge_ctx.v1graphqlq({
                "query": f"mutation($arg: {name}!) {{ alias_direct_{i}(arg: $arg) }}",
                "variables": {"arg": value},
            })
            assert response == {"data": {f"alias_direct_{i}": value}}
        # Boolean is invalid for a reused Postgres bigint. Generic JSON custom
        # scalars would accept it, and the mirror handler would return it.
        response = hge_ctx.v1graphqlq({
            "query": f"mutation($arg: {names[1]}!) {{ alias_direct_1(arg: $arg) }}",
            "variables": {"arg": True},
        })
        assert "errors" in response, response
        assert response["errors"][0]["extensions"]["code"] == "parse-failed", response

    @pytest.mark.parametrize("convention", ["hasura-default", "graphql-default"])
    def test_both_spellings_in_one_schema(self, hge_ctx, original_metadata, handler, convention):
        metadata = original_metadata
        metadata["custom_types"] = {}
        metadata["actions"] = [
            action(f"alias_{spelling}_{i}", f"{name}!", f"{name}!", handler)
            for spelling in ("lower", "title")
            for i, name in enumerate(scalar_names(spelling))
        ]
        set_convention(metadata, convention)
        replace(hge_ctx, metadata)
        for name in SCALARS + tuple(s.title() for s in SCALARS):
            response = hge_ctx.v1graphqlq({
                "query": 'query($name: String!) { __type(name: $name) { name kind } }',
                "variables": {"name": name},
            })
            assert response == {"data": {"__type": {"name": name, "kind": "SCALAR"}}}

    @pytest.mark.parametrize("convention", ["hasura-default", "graphql-default"])
    def test_postgres_and_remote_schema(self, hge_ctx, original_metadata, handler,
                                        scalar_remote_schema, convention):
        metadata = original_metadata
        set_convention(metadata, convention)
        source = next(source for source in metadata["sources"] if source["name"] == "pg1")
        source["tables"] = [{
            "table": {"schema": "public", "name": "alias_scalar_rows"},
            "configuration": {"custom_root_fields": {"select": "alias_scalar_rows"}},
        }]
        metadata["remote_schemas"] = [{
            "name": "scalar_alias_remote",
            "definition": {"url": scalar_remote_schema},
        }]
        # Check PG + remote-schema composition independently before adding
        # actions, so an action validation failure cannot mask that scenario.
        metadata["custom_types"] = {}
        metadata["actions"] = []
        replace(hge_ctx, metadata)
        metadata["custom_types"] = {
            "input_objects": [{
                "name": "AliasInput",
                "fields": [{"name": f"field{i}", "type": f"[{name}!]!"}
                           for i, name in enumerate(SCALARS)],
            }],
            "objects": [{
                "name": "AliasOutput",
                "fields": [{"name": f"field{i}", "type": f"[{name}!]!"}
                           for i, name in enumerate(SCALARS)],
            }],
        }
        metadata["actions"] = [action("alias_echo", "AliasInput!", "AliasOutput!", handler)]
        # Includes strict metadata application, a consistency check, and full
        # introspection/client-schema reconstruction in every environment.
        replace(hge_ctx, metadata)
        payload = {f"field{i}": [value] for i, value in enumerate(VALUES)}
        remote = hge_ctx.v1graphqlq({
            "query": """query($arg: RemoteAliasInput!) {
                remote_scalar_echo(arg: $arg) { field0 field1 field2 field3 }
            }""",
            "variables": {"arg": payload},
        })
        assert remote == {"data": {"remote_scalar_echo": payload}}
        echoed = hge_ctx.v1graphqlq({
            "query": """mutation($arg: AliasInput!) {
                alias_echo(arg: $arg) { field0 field1 field2 field3 }
            }""",
            "variables": {"arg": payload},
        })
        assert echoed == {"data": {"alias_echo": payload}}
        rows = hge_ctx.v1graphqlq({"query": """{
            alias_scalar_rows {
                id payload count created expires
            }
        }"""})
        assert "errors" not in rows, rows
        row, = rows["data"]["alias_scalar_rows"]
        assert row["id"] == 1
        assert row["payload"] == VALUES[0]
        # The test harness enables stringify-numeric-types for PG responses.
        assert str(row["count"]) == "42"
        assert row["created"].startswith(VALUES[2])
        assert row["expires"].startswith(VALUES[3][:-1])

    @pytest.mark.parametrize("convention", ["hasura-default", "graphql-default"])
    def test_explicit_types_win_over_new_aliases(self, hge_ctx, original_metadata, handler, convention):
        metadata = original_metadata
        # Pick the non-native spelling so this exercises NEW alias precedence
        # without changing pre-existing exact-match resolution.
        title = convention == "hasura-default"
        scalar = "Bigint" if title else "bigint"
        enum = "Jsonb" if title else "jsonb"
        input_type = "Timestamp" if title else "timestamp"
        output_type = "Timestamptz" if title else "timestamptz"
        metadata["custom_types"] = {
            "scalars": [{"name": scalar}],
            "enums": [{"name": enum, "values": [{"value": "ONE"}]}],
            "input_objects": [{
                "name": input_type,
                "fields": [{"name": "value", "type": scalar}, {"name": "choice", "type": enum}],
            }],
            "objects": [{
                "name": output_type,
                "fields": [{"name": "value", "type": scalar}, {"name": "choice", "type": enum}],
            }],
        }
        metadata["actions"] = [
            action("alias_custom_scalar", f"{scalar}!", f"{scalar}!", handler),
            action("alias_custom_object", f"{input_type}!", f"{output_type}!", handler),
            action("alias_custom_enum", f"{enum}!", "String!", handler),
        ]
        set_convention(metadata, convention)
        replace(hge_ctx, metadata)
        response = hge_ctx.v1graphqlq({
            "query": f"mutation($arg: {scalar}!) {{ alias_custom_scalar(arg: $arg) }}",
            "variables": {"arg": {"not": "a bigint"}},
        })
        assert response == {"data": {"alias_custom_scalar": {"not": "a bigint"}}}
        payload = {"value": True, "choice": "ONE"}
        response = hge_ctx.v1graphqlq({
            "query": f"mutation($arg: {input_type}!) {{ alias_custom_object(arg: $arg) {{ value choice }} }}",
            "variables": {"arg": payload},
        })
        assert response == {"data": {"alias_custom_object": payload}}
        response = hge_ctx.v1graphqlq({
            "query": f"mutation($arg: {enum}!) {{ alias_custom_enum(arg: $arg) }}",
            "variables": {"arg": "ONE"},
        })
        assert response == {"data": {"alias_custom_enum": "ONE"}}


@pytest.mark.hge_env("HASURA_GRAPHQL_EXPERIMENTAL_FEATURES", "naming_convention")
@pytest.mark.hge_env("HASURA_FF_NAMING_CONVENTION_SEP_2023", "False")
class TestActionScalarAliasesLegacyFlag(ActionScalarAliases):
    pass


@pytest.mark.hge_env("HASURA_GRAPHQL_EXPERIMENTAL_FEATURES", "naming_convention")
@pytest.mark.hge_env("HASURA_FF_NAMING_CONVENTION_SEP_2023", "True")
class TestActionScalarAliasesEnabledFlag(ActionScalarAliases):
    pass

# In --hge-bin mode the fixture starts a separate engine per class and omits
# None-valued markers. This tests an absent experimental entry, independently
# of any environment inherited by the pytest parent process.
@pytest.mark.hge_env("HASURA_GRAPHQL_EXPERIMENTAL_FEATURES", None)
@pytest.mark.hge_env("HASURA_FF_NAMING_CONVENTION_SEP_2023", "False")
class TestActionScalarAliasesLegacyFlagNoExperimental(ActionScalarAliases):
    pass


@pytest.mark.hge_env("HASURA_GRAPHQL_EXPERIMENTAL_FEATURES", None)
@pytest.mark.hge_env("HASURA_FF_NAMING_CONVENTION_SEP_2023", "True")
class TestActionScalarAliasesEnabledFlagNoExperimental(ActionScalarAliases):
    pass
