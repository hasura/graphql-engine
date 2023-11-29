from ruamel.yaml import YAML
import pytest

from conftest import extract_server_address_from
from remote_server import NodeGraphQL
from validate import check_query_f, check_query

yaml=YAML(typ='safe', pure=True)

@pytest.fixture(scope='class')
@pytest.mark.early
def graphql_service(worker_id: str, hge_fixture_env: dict[str, str]):
    (_, port) = extract_server_address_from('GRAPHQL_SERVICE_HANDLER')
    server = NodeGraphQL(worker_id, 'remote_schemas/nodejs/index.js', port=port)
    server.start()
    print(f'{graphql_service.__name__} server started on {server.url}')
    hge_fixture_env['GRAPHQL_SERVICE_HANDLER'] = server.url
    yield server
    server.stop()

@pytest.mark.usefixtures('per_class_tests_db_state')
class TestGraphqlIntrospection:

    def test_introspection(self, hge_ctx):
        with open(self.dir() + "/introspection.yaml") as c:
            conf = yaml.load(c)
        resp, _ = check_query(hge_ctx, conf)
        hasArticle = False
        hasArticleAuthorFKRel = False
        hasArticleAuthorManualRel = False
        for t in resp['data']['__schema']['types']:
            if t['name'] == 'article':
                hasArticle = True
                for fld in t['fields']:
                    if fld['name'] == 'author_obj_rel_manual':
                        hasArticleAuthorManualRel = True
                        assert fld['type']['kind'] == 'OBJECT'
                    elif fld['name'] == 'author_obj_rel_fk':
                        hasArticleAuthorFKRel = True
                        assert fld['type']['kind'] == 'NON_NULL'
        assert hasArticle
        assert hasArticleAuthorFKRel
        assert hasArticleAuthorManualRel

    def test_introspection_user(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/introspection_user_role.yaml")

    @classmethod
    def dir(cls):
        return "queries/graphql_introspection"


@pytest.mark.usefixtures('per_class_tests_db_state')
class TestNullableObjectRelationshipInSchema:
    @classmethod
    def dir(cls):
        return "queries/graphql_introspection/nullable_object_relationship"

    def test_introspection_both_directions_both_nullabilities(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/nullability.yaml")

def getTypeNameFromType(typeObject):
    if typeObject['name'] != None:
        return typeObject['name']
    elif isinstance(typeObject['ofType'],dict):
        return getTypeNameFromType(typeObject['ofType'])
    else:
        raise Exception("typeObject doesn't have name and ofType is not an object")

@pytest.mark.usefixtures('per_class_tests_db_state', 'graphql_service')
class TestRemoteRelationshipsGraphQLNames:
    @classmethod
    def dir(cls):
        return "queries/graphql_introspection/remote_relationships"

    def test_relation_from_custom_schema_has_correct_name(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/relation_custom_schema_has_correct_name.yaml")

@pytest.mark.usefixtures('per_class_tests_db_state')
class TestGraphqlIntrospectionWithCustomTableName:

    # test to check some of the type names that are generated
    # while tracking a table with a custom name
    def test_introspection(self, hge_ctx):
        with open(self.dir() + "/introspection.yaml") as c:
            conf = yaml.load(c)
        resp, _ = check_query(hge_ctx, conf)
        hasMultiSelect = False
        hasAggregate = False
        hasSelectByPk = False
        hasQueryRoot = False
        hasMultiInsert = False
        hasUpdateByPk = False
        for t in resp['data']['__schema']['types']:
            if t['name'] == 'query_root':
                hasQueryRoot = True
                for field in t['fields']:
                    if field['name'] == 'user_address':
                        hasMultiSelect = True
                        assert 'args' in field
                        for args in field['args']:
                            if args['name'] == 'distinct_on':
                                assert "user_address_select_column" == getTypeNameFromType(args['type'])
                            elif args['name'] == 'order_by':
                                assert "user_address_order_by" == getTypeNameFromType(args['type'])
                            elif args['name'] == 'where':
                                assert 'user_address_bool_exp' == getTypeNameFromType(args['type'])
                    elif field['name'] == 'user_address_aggregate':
                        hasAggregate = True
                        assert "user_address_aggregate" == getTypeNameFromType(field['type'])
                    elif field['name'] == 'user_address_by_pk':
                        assert "user_address" == getTypeNameFromType(field['type'])
                        hasSelectByPk = True
            elif t['name'] == 'mutation_root':
                for field in t['fields']:
                    if field['name'] == 'insert_user_address':
                        hasMultiInsert = True
                        assert "user_address_mutation_response" == getTypeNameFromType(field['type'])
                        for args in field['args']:
                            if args['name'] == 'object':
                                assert "user_address_insert_input" == getTypeNameFromType(args['type'])
                    elif field['name'] == 'update_user_address_by_pk':
                        hasUpdateByPk = True
                        assert "user_address" == getTypeNameFromType(field['type'])
                        for args in field['args']:
                            if args['name'] == 'object':
                                assert "user_address" == getTypeNameFromType(args['type'])
        assert hasQueryRoot
        assert hasMultiSelect
        assert hasAggregate
        assert hasSelectByPk
        assert hasMultiInsert
        assert hasUpdateByPk

    @classmethod
    def dir(cls):
        return "queries/graphql_introspection/custom_table_name"

@pytest.mark.usefixtures('per_class_tests_db_state', 'pro_tests_fixtures')
class TestDisableGraphQLIntrospection:

    @classmethod
    def dir(cls):
        return "queries/graphql_introspection/disable_introspection"

    setup_metadata_api_version = "v2"

    def test_disable_introspection(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/disable_introspection.yaml")

@pytest.mark.usefixtures('per_class_tests_db_state')
class TestGraphQlIntrospectionDescriptions:

    setup_metadata_api_version = "v2"

    @classmethod
    def dir(cls):
        return "queries/graphql_introspection/descriptions"

    def test_automatic_comment_in_db(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/automatic_comment_in_db.yaml")

    def test_automatic_no_comment_in_db(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/automatic_no_comment_in_db.yaml")

    def test_explicit_comment_in_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/explicit_comment_in_metadata.yaml")

    def test_explicit_no_comment_in_metadata(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/explicit_no_comment_in_metadata.yaml")

    def test_root_fields(self, hge_ctx):
        check_query_f(hge_ctx, self.dir() + "/root_fields.yaml")
