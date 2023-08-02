#!/usr/bin/env python3

import pytest
from validate import check_query, check_query_f

usefixtures = pytest.mark.usefixtures

pytestmark = [
    pytest.mark.hge_env('HASURA_GRAPHQL_ENABLE_ALLOWLIST', 'true')
]

@pytest.mark.parametrize("transport", ['http', 'websocket'])
@usefixtures('per_class_tests_db_state')
class TestAllowlistQueries:

    def test_query_user(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_user.yaml', transport)

    def test_query_user_by_pk(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_user_by_pk.yaml', transport)

    def test_query_user_with_typename(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_user_with_typename.yaml', transport)

    def test_query_non_allowlist(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_non_allowlist.yaml', transport)

    def test_query_user_fragment(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_user_fragment.yaml', transport)

    def test_query_as_admin(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_as_admin.yaml', transport)

    def test_update_query(self, hge_ctx, transport):
        # test only for http
        if transport != 'http':
            pytest.skip("http-only")
        check_query_f(hge_ctx, self.dir() + '/update_query.yaml', transport)
        check_query_f(hge_ctx, self.dir() + '/add_duplicate_query.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/allowlist'


def export_metadata(hge_ctx):
    resp = hge_ctx.v1metadataq({
        'type': 'export_metadata',
        'version': 2,
        'args': {}
    })
    return resp["metadata"]

def assert_allowlist_metadata(hge_ctx, expected):
    metadata = export_metadata(hge_ctx)
    assert metadata.get("allowlist") == expected

def replace_allowlist_metadata_with_check(hge_ctx, base_metadata, allowlist, expected_status_code):
    metadata = dict(base_metadata)
    metadata["allowlist"] = allowlist
    return hge_ctx.v1metadataq({
        'type': 'replace_metadata',
        'version': 2,
        'args': {
            'metadata': metadata
        }
    }, expected_status_code = expected_status_code)

def replace_allowlist_metadata(hge_ctx, base_metadata, allowlist):
    return replace_allowlist_metadata_with_check(hge_ctx, base_metadata, allowlist, expected_status_code = 200)

def fail_replace_allowlist_metadata(hge_ctx, base_metadata, allowlist, expected_error):
    resp = replace_allowlist_metadata_with_check(hge_ctx, base_metadata, allowlist, expected_status_code = 400)
    assert resp == expected_error

def add_collection_to_allowlist_with_check(hge_ctx, args, expected_status_code):
    payload = {
        "type": "add_collection_to_allowlist",
        "args": args,
    }
    return hge_ctx.v1metadataq(payload, expected_status_code = expected_status_code)

def add_collection_to_allowlist(hge_ctx, args):
    return add_collection_to_allowlist_with_check(hge_ctx, args, expected_status_code = 200)

def fail_add_collection_to_allowlist(hge_ctx, args, expected_error):
    resp = add_collection_to_allowlist_with_check(hge_ctx, args, expected_status_code = 400)
    assert resp == expected_error

def update_scope_of_collection_in_allowlist_with_check(hge_ctx, args, expected_status_code):
    payload = {
        "type": "update_scope_of_collection_in_allowlist",
        "args": args,
    }
    return hge_ctx.v1metadataq(payload, expected_status_code = expected_status_code)

def update_scope_of_collection_in_allowlist(hge_ctx, args):
    update_scope_of_collection_in_allowlist_with_check(hge_ctx, args, expected_status_code = 200)

def fail_update_scope_of_collection_in_allowlist(hge_ctx, args, expected_error):
    resp = update_scope_of_collection_in_allowlist_with_check(hge_ctx, args, expected_status_code = 400)
    assert resp == expected_error

def drop_collection_from_allowlist_with_check(hge_ctx, args, expected_status_code):
    payload = {
        "type": "drop_collection_from_allowlist",
        "args": args
    }
    return hge_ctx.v1metadataq(payload, expected_status_code = expected_status_code)

def drop_collection_from_allowlist(hge_ctx, args):
    return drop_collection_from_allowlist_with_check(hge_ctx, args, expected_status_code = 200)

def fail_drop_collection_from_allowlist(hge_ctx, args, expected_error):
    resp = drop_collection_from_allowlist_with_check(hge_ctx, args, expected_status_code = 400)
    assert resp == expected_error

@pytest.fixture(scope="function")
def clean_allowlist(hge_ctx):
    yield
    drop_collection_from_allowlist_with_check(hge_ctx, {"collection": "collection_1"}, expected_status_code = None)
    drop_collection_from_allowlist_with_check(hge_ctx, {"collection": "collection_2"}, expected_status_code = None)

@usefixtures('clean_allowlist', 'per_class_tests_db_state')
class TestAllowlistMetadata:
    @classmethod
    def dir(cls):
        return 'queries/graphql_query/allowlist_role_based'

    def test_rename_query_collection(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + '/rename_query_collection.yaml')

    def test_add_update_drop(self, hge_ctx):
        # Cycle through add_collection_to_allowlist,
        # update_scope_of_collection_in_allowlist and
        # drop_collection_from_allowlist, verifying
        # state of metadata after each step.

        assert_allowlist_metadata(hge_ctx, None)

        add_collection_to_allowlist(hge_ctx, {
            "collection": "collection_1",
            "scope": {
                "global": True
            }
        })

        assert_allowlist_metadata(hge_ctx, [{
            "collection": "collection_1",
            "scope": {
                "global": True
            }
        }])

        add_collection_to_allowlist(hge_ctx, {
            "collection": "collection_2",
            "scope": {
                "global": False,
                "roles": ["foo", "bar"]
            }
        })

        assert_allowlist_metadata(hge_ctx, [{
            "collection": "collection_1",
            "scope": {
                "global": True
            }
        }, {
            "collection": "collection_2",
            "scope": {
                "global": False,
                "roles": ["foo", "bar"]
            }
        }])

        update_scope_of_collection_in_allowlist(hge_ctx, {
            "collection": "collection_1",
            "scope": {
                "global": False,
                "roles": ["baz"]
            }
        })
        update_scope_of_collection_in_allowlist(hge_ctx, {
            "collection": "collection_2",
            "scope": {
                "global": True
            }
        })

        assert_allowlist_metadata(hge_ctx, [{
            "collection": "collection_1",
            "scope": {
                "global": False,
                "roles": ["baz"]
            }
        }, {
            "collection": "collection_2",
            "scope": {
                "global": True
            }
        }])

        drop_collection_from_allowlist(hge_ctx, {"collection": "collection_1"})

        assert_allowlist_metadata(hge_ctx, [{
            "collection": "collection_2",
            "scope": {
                "global": True
            }
        }])

        drop_collection_from_allowlist(hge_ctx, {"collection": "collection_2"})

        assert_allowlist_metadata(hge_ctx, None)

    def test_add_validation(self, hge_ctx, request):
        # collection required
        fail_add_collection_to_allowlist(hge_ctx, {
            "scope": {
                "global": True,
            }
        }, {
            "path": "$.args",
            "error": 'key "collection" not found',
            "code": "parse-failed",
        })

        # when global is false, roles needs to be present
        fail_add_collection_to_allowlist(hge_ctx, {
            "collection": "collection_1",
            "scope": {
                "global": False,
            }
        }, {
            "path": "$.args.scope",
            "error": 'key "roles" not found',
            "code": "parse-failed",
        })

        # when global is false, roles needs to be non-empty
        fail_add_collection_to_allowlist(hge_ctx, {
            "collection": "collection_1",
            "scope": {
                "global": False,
                "roles": []
            }
        }, {
            "path": "$.args.scope.roles",
            "error": "parsing NonEmpty failed, unexpected empty list",
            "code": "parse-failed",
        })

        # when global is true, there can't be roles
        fail_add_collection_to_allowlist(hge_ctx, {
            "collection": "collection_1",
            "scope": {
                "global": True,
                "roles": ["foo", "bar"],
            }
        }, {
            "path": "$.args.scope",
            "error": "roles are not allowed when global is true",
            "code": "parse-failed",
        })

        # if scope is given, global is required
        fail_add_collection_to_allowlist(hge_ctx, {
            "collection": "collection_1",
            "scope": {}
        }, {
            "path": "$.args.scope",
            "error": 'key "global" not found',
            "code": "parse-failed",
        })

    def test_update_validation(self, hge_ctx):
        # collection required
        fail_update_scope_of_collection_in_allowlist(hge_ctx, {
            "scope": {
                "global": True,
            }
        }, {
            "path": "$.args",
            "error": 'key "collection" not found',
            "code": "parse-failed",
        })

        # scope required
        fail_update_scope_of_collection_in_allowlist(hge_ctx, {
            "collection": "collection_1"
        }, {
            "path": "$.args",
            "error": 'key "scope" not found',
            "code": "parse-failed",
        })

        # we could cover most of the failure cases from test_add_validation

    def test_drop_validation(self, hge_ctx):
        # collection required
        fail_drop_collection_from_allowlist(hge_ctx, {
        }, {
            "path": "$.args",
            "error": "parsing Hasura.RQL.Types.Allowlist.DropCollectionFromAllowlist(DropCollectionFromAllowlist) failed, key \"collection\" not found",
            "code": "parse-failed",
        })

    def test_replace_validation(self, hge_ctx):
        base_metadata = export_metadata(hge_ctx)

        # duplicate collections aren't allowed
        fail_replace_allowlist_metadata(hge_ctx, base_metadata, [{
            "collection": "collection_1",
        }, {
            "collection": "collection_1",
            "scope": {
                "global": True
            }
        }], {
            "path": "$.args.metadata",
            "error": 'multiple declarations exist for the following allowlist entries: collection_1',
            "code": "parse-failed",
        })

    def test_add_scope_defaults_to_global(self, hge_ctx):
        add_collection_to_allowlist(hge_ctx, {
            "collection": "collection_1",
        })

        assert_allowlist_metadata(hge_ctx, [{
            "collection": "collection_1",
            "scope": {
                "global": True
            }
        }])

    def test_drop_fail_when_missing(self, hge_ctx):
        fail_drop_collection_from_allowlist(hge_ctx, {
            "collection": "collection_1"
        }, {
            'path': '$.args',
            'error': 'collection "collection_1" doesn\'t exist in the allowlist',
            'code': 'not-found'
        })

    def test_update_fail_if_missing(self, hge_ctx):
        fail_update_scope_of_collection_in_allowlist(hge_ctx, {
            "collection": "collection_1",
            "scope": {
                "global": True
            }
        }, {
            'path': '$.args',
            'error': 'collection "collection_1" doesn\'t exist in the allowlist',
            'code': 'not-found'
        })

    def test_add_again(self, hge_ctx):
        # re-adding doesn't fail (compare RFC), instead points at change scope
        orig = {
            "collection": "collection_1",
            "scope": {
                "global": True
            }
        }
        add_collection_to_allowlist(hge_ctx, orig)
        assert_allowlist_metadata(hge_ctx, [orig])

        resp = add_collection_to_allowlist(hge_ctx, {
            "collection": "collection_1",
            "scope": {
                "global": False,
                "roles": ["foo"]
            }
        })
        assert resp == {
            "message": 'collection "collection_1" already exists in the allowlist, scope ignored; to change scope, use update_scope_of_collection_in_allowlist'
        }
        assert_allowlist_metadata(hge_ctx, [orig])

        orig2 = {
            "collection": "collection_2",
            "scope": {
                "global": False,
                "roles": ["foo"]
            }
        }
        add_collection_to_allowlist(hge_ctx, orig2)
        assert_allowlist_metadata(hge_ctx, [orig, orig2])

        resp = add_collection_to_allowlist(hge_ctx, {
            "collection": "collection_2",
            "scope": {
                "global": True
            }
        })
        assert resp == {
            "message": 'collection "collection_2" already exists in the allowlist, scope ignored; to change scope, use update_scope_of_collection_in_allowlist'
        }
        assert_allowlist_metadata(hge_ctx, [orig, orig2])


# query_0 is in no collection
query_0 = """
  query {
    author {
      first_name
    }
  }
"""
response_0 = {
    "data": {
        "author": [
            {"first_name": "George"},
            {"first_name": "Paulo"},
        ],
    },
}

# query_1 is only in collection_1
query_1 = """
  query {
    author {
      first_name
      last_name
    }
  }
"""
response_1 = {
    "data": {
        "author": [
            {"first_name": "George", "last_name": "Martin"},
            {"first_name": "Paulo", "last_name": "Coelho"},
        ],
    },
}

# query_2 is in both collections
query_2 = """
  query {
    author {
      __typename
      last_name
    }
  }
"""
response_2 = {
    "data": {
        "author": [
            {"__typename": "author", "last_name": "Martin"},
            {"__typename": "author", "last_name": "Coelho"}
        ]
    }
}

# query_3 is only in collection_2
query_3 = """
  query {
    author {
      id
    }
  }
"""
response_3 = {
    "data": {
        "author": [
            {"id": 1},
            {"id": 2}
        ],
    },
}

response_not_allowed = {
    "errors": [
        {
            "extensions": {
                "path": "$",
                "code": "validation-failed",
            },
            "message": "query is not allowed",
        },
    ],
}

def assert_query_allowed(hge_ctx, role, query, response):
    check_query(hge_ctx, {
        "url": "/v1/graphql",
        "status": 200,
        "headers": {"x-hasura-role": role},
        "response": response,
        "query": {"query": query},
    })

def assert_query_not_allowed(hge_ctx, role, query):
    check_query(hge_ctx, {
        "url": "/v1/graphql",
        "status": 200,
        "headers": {"x-hasura-role": role},
        "response": response_not_allowed,
        "query": {"query": query},
    })

@usefixtures('clean_allowlist', 'per_class_tests_db_state')
class TestRoleBasedAllowlistQueries:
    @classmethod
    def dir(cls):
        return 'queries/graphql_query/allowlist_role_based'

    def test_baseline(self, hge_ctx):
        for role in ["user", "guest"]:
            assert_query_not_allowed(hge_ctx, role, query_0)
            assert_query_not_allowed(hge_ctx, role, query_1)
            assert_query_not_allowed(hge_ctx, role, query_2)
            assert_query_not_allowed(hge_ctx, role, query_3)
        assert_query_allowed(hge_ctx, "admin", query_0, response_0)
        assert_query_allowed(hge_ctx, "admin", query_1, response_1)
        assert_query_allowed(hge_ctx, "admin", query_2, response_2)
        assert_query_allowed(hge_ctx, "admin", query_3, response_3)

    def test_allowlists(self, hge_ctx):
        add_collection_to_allowlist(hge_ctx, {
            "collection": "collection_1",
            "scope": {
                "global": True,
            }
        })
        add_collection_to_allowlist(hge_ctx, {
            "collection": "collection_2",
            "scope": {
                "global": False,
                "roles": [
                    "user",
                    "editor"
                ]
            }
        })

        for role in ["user", "guest"]:
          assert_query_not_allowed(hge_ctx, role, query_0)
          assert_query_allowed(hge_ctx, role, query_1, response_1)
          assert_query_allowed(hge_ctx, role, query_2, response_2)

        assert_query_not_allowed(hge_ctx, "guest", query_3)

        if hge_ctx.pro_tests:
            assert_query_allowed(hge_ctx, "user", query_3, response_3)
        else:
            assert_query_not_allowed(hge_ctx, "user", query_3)
