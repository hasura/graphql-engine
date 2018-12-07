
import yaml
import pytest
import jwt
from validate import check_query

if not pytest.config.getoption("--hge-jwt-key-file"):
    pytest.skip("--hge-jwt-key-file is missing, skipping JWT basic tests", allow_module_level=True)


class TestJWTBasic:

    def test_jwt_invalid_role_in_request_header(self, hge_ctx):
        self.claims['https://hasura.io/jwt/claims'] = {
            "x-hasura-user-id": "1",
            "x-hasura-allowed-roles": ["contractor", "editor"],
            "x-hasura-default-role": "contractor"
        }
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512' ).decode('UTF-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['response'] = {
            'errors': [{
                'extensions': {
                    'code': 'access-denied',
                    'path': '$'
                },
                'message': 'Your current role is not in allowed roles'
            }]
        }
        self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_no_allowed_roles_in_claim(self, hge_ctx):
        self.claims['https://hasura.io/jwt/claims'] = {
            "x-hasura-user-id": "1",
            "x-hasura-default-role": "user"
        }
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512' ).decode('UTF-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['response'] = {
            'errors': [{
                'extensions': {
                    'code': 'jwt-missing-role-claims',
                    'path': '$'
                },
                'message': 'JWT claim does not contain x-hasura-allowed-roles'
            }]
        }
        self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_invalid_allowed_roles_in_claim(self, hge_ctx):
        self.claims['https://hasura.io/jwt/claims'] = {
            "x-hasura-user-id": "1",
            "x-hasura-allowed-roles": "user",
            "x-hasura-default-role": "user"
        }
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512' ).decode('UTF-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['response'] = {
            'errors': [{
                'extensions': {
                    'code': 'jwt-invalid-claims',
                    'path': '$'
                },
                'message': 'invalid x-hasura-allowed-roles; should be a list of roles'
            }]
        }
        self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_no_default_role(self, hge_ctx):
        self.claims['https://hasura.io/jwt/claims'] = {
            "x-hasura-user-id": "1",
            "x-hasura-allowed-roles": ["user"],
        }
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512' ).decode('UTF-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['response'] = {
            'errors': [{
                'extensions': {
                    'code': 'jwt-missing-role-claims',
                    'path': '$'
                },
                'message': 'JWT claim does not contain x-hasura-default-role'
            }]
        }
        self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False)

    @pytest.fixture(autouse=True)
    def transact(self, request, hge_ctx):
        self.dir = 'queries/graphql_query/permissions'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        with open(self.dir + '/user_select_query_unpublished_articles.yaml') as c:
            self.conf = yaml.safe_load(c)
        self.claims = {
            "sub": "1234567890",
            "name": "John Doe",
            "iat": 1516239022
        }
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp
