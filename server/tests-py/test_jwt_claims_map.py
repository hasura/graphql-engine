import pytest
import jwt
import math
import ruamel.yaml as yaml
import json

from validate import check_query
from datetime import datetime, timedelta
from context import PytestConf

if not PytestConf.config.getoption('--hge-jwt-key-file'):
    pytest.skip('--hge-jwt-key-file is missing, skipping JWT tests', allow_module_level=True)

hge_jwt_conf = PytestConf.config.getoption('--hge-jwt-conf')

if not hge_jwt_conf:
    pytest.skip('--hge-jwt-key-conf is missing, skipping JWT tests', allow_module_level=True)

if 'claims_map' not in hge_jwt_conf:
    pytest.skip('cliams_map missing in jwt config, skipping JWT Claims Map tests', allow_module_level=True)

# The following claims_map is assumed to be set
# {
#     "claims_map": {
#         "x-hasura-user-id": {"path":"$.['https://myapp.com/jwt/claims'].user.id"}
#         "x-hasura-allowed-roles": {"$.['https://myapp.com/jwt/claims'].role.allowed","default":["user","editor"]}
#         "x-hasura-default-role": {"$.['https://myapp.com/jwt/claims'].role.default","default":"user"}
#     }
# }

def clean_null_terms(d):
   clean = {}
   for k, v in d.items():
      if isinstance(v, dict):
         nested = clean_null_terms(v)
         if len(nested.keys()) > 0:
            clean[k] = nested
      elif v is not None:
         clean[k] = v
   return clean

# TestJWTClaimsMapBasic will be called using two different JWT configs
# one with default values and the other without default values. The
# default values here is referred to the default value that's being
# used when a value is not found while looking up the JWT token using
# the JSON Path provided
@pytest.mark.parametrize('endpoint', ['/v1/graphql', '/v1alpha1/graphql'])
class TestJWTClaimsMapBasic():
    def mk_claims(self, user_id=None, allowed_roles=None, default_role=None):
        self.claims['https://myapp.com/jwt/claims'] = clean_null_terms({
            'user': {
                'id': user_id
            },
            'role': {
                'allowed': allowed_roles,
                'default': default_role
            }
        })

    def test_jwt_claims_map_valid_claims_success(self, hge_ctx, endpoint):
        self.mk_claims('1', ['user', 'editor'], 'user')
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['url'] = endpoint
        self.conf['status'] = 200
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_claims_map_invalid_role_in_request_header(self, hge_ctx, endpoint):
        self.mk_claims('1', ['contractor', 'editor'], 'contractor')
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['response'] = {
            'errors': [{
                'extensions': {
                    'code': 'access-denied',
                    'path': '$'
                },
                'message': 'Your requested role is not in allowed roles'
            }]
        }
        self.conf['url'] = endpoint
        if endpoint == '/v1/graphql':
            self.conf['status'] = 200
        if endpoint == '/v1alpha1/graphql':
            self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_claims_map_no_allowed_roles_in_claim(self, hge_ctx, endpoint):
        self.mk_claims('1', None, 'user')
        default_allowed_roles = hge_ctx.hge_jwt_conf_dict['claims_map']['x-hasura-allowed-roles'].get('default')
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        if default_allowed_roles is None:
            self.conf['response'] = {
                'errors': [{
                    'extensions': {
                        'code': 'jwt-missing-role-claims',
                        'path': '$'
                    },
                    'message': 'JWT claim does not contain x-hasura-allowed-roles'
                }]
            }
            self.conf['url'] = endpoint
            if endpoint == '/v1/graphql':
                self.conf['status'] = 200
            if endpoint == '/v1alpha1/graphql':
                self.conf['status'] = 400
        else:
            self.conf['status'] = 200
        self.conf['url'] = endpoint
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_claims_map_invalid_allowed_roles_in_claim(self, hge_ctx, endpoint):
        self.mk_claims('1', 'user', 'user')
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['response'] = {
            'errors': [{
                'extensions': {
                    'code': 'jwt-invalid-claims',
                    'path': '$'
                },
                'message': 'invalid x-hasura-allowed-roles; should be a list of roles: parsing [] failed, expected Array, but encountered String'
            }]
        }
        self.conf['url'] = endpoint
        if endpoint == '/v1/graphql':
            self.conf['status'] = 200
        if endpoint == '/v1alpha1/graphql':
            self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_claims_map_no_default_role(self, hge_ctx, endpoint):
        # default_default_role is the default default role set in the JWT config
        # when the lookup with the JSONPath fails, this is the value that will
        # be used for the `x-hasura-default-role` claim
        default_default_role = hge_ctx.hge_jwt_conf_dict['claims_map']['x-hasura-default-role'].get('default')
        self.mk_claims('1', ['user'])
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        if default_default_role is None:
            self.conf['response'] = {
                'errors': [{
                    'extensions': {
                        'code': 'jwt-missing-role-claims',
                        'path': '$'
                    },
                    'message': 'JWT claim does not contain x-hasura-default-role'
                }]
            }
            if endpoint == '/v1/graphql':
                self.conf['status'] = 200
            if endpoint == '/v1alpha1/graphql':
                self.conf['status'] = 400
        else:
            self.conf['status'] = 200
        self.conf['url'] = endpoint
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_claims_map_claim_not_found(self, hge_ctx, endpoint):
        default_user_id = hge_ctx.hge_jwt_conf_dict['claims_map']['x-hasura-user-id'].get('default')
        self.mk_claims(None, ['user', 'editor'], 'user')
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        if default_user_id is None:
            self.conf['response'] = {
                'errors': [{
                    'extensions': {
                        'code': 'jwt-invalid-claims',
                        'path': '$'
                    },
                    'message': 'JWT claim from claims_map, x-hasura-user-id not found'
                }]
            }
            if endpoint == '/v1/graphql':
                self.conf['status'] = 200
            if endpoint == '/v1alpha1/graphql':
                self.conf['status'] = 400
        else:
            self.conf['status'] = 200
        self.conf['url'] = endpoint
        check_query(hge_ctx, self.conf, add_auth=False)

    @pytest.fixture(autouse=True)
    def transact(self, setup):
        self.dir = 'queries/graphql_query/permissions'
        with open(self.dir + '/user_select_query_unpublished_articles.yaml') as c:
            self.conf = yaml.safe_load(c)
        curr_time = datetime.now()
        exp_time = curr_time + timedelta(hours=1)
        self.claims = {
            'sub': '1234567890',
            'name': 'John Doe',
            'iat': math.floor(curr_time.timestamp()),
            'exp': math.floor(exp_time.timestamp())
        }

    @pytest.fixture(scope='class')
    def setup(self, request, hge_ctx):
        self.dir = 'queries/graphql_query/permissions'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp

# The values of 'x-hasura-allowed-roles' and 'x-hasura-default-role' has
# been set in the JWT config
@pytest.mark.parametrize('endpoint', ['/v1/graphql', '/v1alpha1/graphql'])
class TestJWTClaimsMapWithStaticHasuraClaimsMapValues():
    def mk_claims(self, user_id=None):
        self.claims['https://myapp.com/jwt/claims'] = clean_null_terms({
            'user': {
                'id': user_id
            }
        })

    def test_jwt_claims_map_valid_claims_success(self, hge_ctx, endpoint):
        self.mk_claims('1')
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['headers']['x-hasura-custom-header'] = 'custom-value'
        self.conf['url'] = endpoint
        self.conf['status'] = 200
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_claims_map_invalid_role_in_request_header(self, hge_ctx, endpoint):
        self.mk_claims('1')
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['headers']['X-Hasura-Role'] = 'random_string'
        self.conf['response'] = {
            'errors': [{
                'extensions': {
                    'code': 'access-denied',
                    'path': '$'
                },
                'message': 'Your requested role is not in allowed roles'
            }]
        }
        self.conf['url'] = endpoint
        if endpoint == '/v1/graphql':
            self.conf['status'] = 200
        if endpoint == '/v1alpha1/graphql':
            self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_claims_map_claim_not_found(self, hge_ctx, endpoint):
        self.mk_claims(None)
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['response'] = {
            'errors': [{
                'extensions': {
                    'code': 'jwt-invalid-claims',
                    'path': '$'
                },
                'message': 'JWT claim from claims_map, x-hasura-user-id not found'
            }]
        }
        self.conf['url'] = endpoint
        if endpoint == '/v1/graphql':
            self.conf['status'] = 200
        if endpoint == '/v1alpha1/graphql':
            self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False)

    @pytest.fixture(autouse=True)
    def transact(self, setup):
        self.dir = 'queries/graphql_query/permissions'
        with open(self.dir + '/user_select_query_unpublished_articles.yaml') as c:
            self.conf = yaml.safe_load(c)
        curr_time = datetime.now()
        exp_time = curr_time + timedelta(hours=1)
        self.claims = {
            'sub': '1234567890',
            'name': 'John Doe',
            'iat': math.floor(curr_time.timestamp()),
            'exp': math.floor(exp_time.timestamp())
        }

    @pytest.fixture(scope='class')
    def setup(self, request, hge_ctx):
        self.dir = 'queries/graphql_query/permissions'
        st_code, resp = hge_ctx.v1q_f(self.dir + '/setup.yaml')
        assert st_code == 200, resp
        yield
        st_code, resp = hge_ctx.v1q_f(self.dir + '/teardown.yaml')
        assert st_code == 200, resp
