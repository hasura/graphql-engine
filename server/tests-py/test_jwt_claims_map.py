from datetime import datetime, timedelta
import math

import jwt
import pytest
from ruamel.yaml import YAML

from validate import check_query

yaml = YAML(typ='safe', pure=True)

basic_claims_map = {
    'x-hasura-user-id': {
        'path': "$.['https://myapp.com/jwt/claims'].user.id"
    },
    'x-hasura-allowed-roles': {
        'path': "$.['https://myapp.com/jwt/claims'].role.allowed"
    },
    'x-hasura-default-role': {
        'path': "$.['https://myapp.com/jwt/claims'].role.default"
    }
}

basic_claims_map_with_default_values = {
    'x-hasura-user-id': {
        'path': "$.['https://myapp.com/jwt/claims'].user.id",
        'default': '1'
    },
    'x-hasura-allowed-roles': {
        'path': "$.['https://myapp.com/jwt/claims'].role.allowed",
        'default': ['user', 'editor']
    },
    'x-hasura-default-role': {
        'path': "$.['https://myapp.com/jwt/claims'].role.default",
        'default': 'user'
    }
}

static_claims_map = {
    'x-hasura-user-id': {
        'path': "$.['https://myapp.com/jwt/claims'].user.id"
    },
    'x-hasura-allowed-roles': ['user','editor'],
    'x-hasura-default-role': 'user',
    'x-hasura-custom-header': 'custom-value'
}

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
@pytest.mark.admin_secret
@pytest.mark.parametrize('endpoint', ['/v1/graphql', '/v1alpha1/graphql'])
class AbstractTestJWTClaimsMapBasic:
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

    def test_jwt_claims_map_valid_claims_success(self, hge_ctx, jwt_configuration, endpoint):
        self.mk_claims('1', ['user', 'editor'], 'user')
        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['url'] = endpoint
        self.conf['status'] = 200
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_claims_map_invalid_role_in_request_header(self, hge_ctx, jwt_configuration, endpoint):
        self.mk_claims('1', ['contractor', 'editor'], 'contractor')
        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
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

    def test_jwt_claims_map_no_allowed_roles_in_claim(self, hge_ctx, jwt_configuration, endpoint):
        self.mk_claims('1', None, 'user')
        default_allowed_roles = jwt_configuration.server_configuration['claims_map']['x-hasura-allowed-roles'].get('default')
        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
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

    def test_jwt_claims_map_invalid_allowed_roles_in_claim(self, hge_ctx, jwt_configuration, endpoint):
        self.mk_claims('1', 'user', 'user')
        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
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

    def test_jwt_claims_map_no_default_role(self, hge_ctx, jwt_configuration, endpoint):
        # default_default_role is the default default role set in the JWT config
        # when the lookup with the JSONPath fails, this is the value that will
        # be used for the `x-hasura-default-role` claim
        default_default_role = jwt_configuration.server_configuration['claims_map']['x-hasura-default-role'].get('default')
        self.mk_claims('1', ['user'])
        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
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

    def test_jwt_claims_map_claim_not_found(self, hge_ctx, jwt_configuration, endpoint):
        default_user_id = jwt_configuration.server_configuration['claims_map']['x-hasura-user-id'].get('default')
        self.mk_claims(None, ['user', 'editor'], 'user')
        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
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
            self.conf = yaml.load(c)
        curr_time = datetime.now()
        exp_time = curr_time + timedelta(hours=1)
        self.claims = {
            'sub': '1234567890',
            'name': 'John Doe',
            'iat': math.floor(curr_time.timestamp()),
            'exp': math.floor(exp_time.timestamp())
        }

    @pytest.fixture(scope='class')
    def setup(self, postgis, hge_ctx):
        self.dir = 'queries/graphql_query/permissions'
        hge_ctx.v1q_f(self.dir + '/setup.yaml')
        yield
        hge_ctx.v1q_f(self.dir + '/teardown.yaml')


@pytest.mark.jwt('rsa', { 'claims_map': basic_claims_map })
class TestJWTClaimsMapBasicWithRSA(AbstractTestJWTClaimsMapBasic):
    pass


@pytest.mark.jwt('ed25519', { 'claims_map': basic_claims_map })
class TestJWTClaimsMapBasicWithEd25519(AbstractTestJWTClaimsMapBasic):
    pass


@pytest.mark.jwt('rsa', { 'claims_map': basic_claims_map_with_default_values })
class TestJWTClaimsMapBasicWithRSAAndDefaultValues(AbstractTestJWTClaimsMapBasic):
    pass


@pytest.mark.jwt('ed25519', { 'claims_map': basic_claims_map_with_default_values })
class TestJWTClaimsMapBasicWithEd25519AndDefaultValues(AbstractTestJWTClaimsMapBasic):
    pass


# The values of 'x-hasura-allowed-roles' and 'x-hasura-default-role' has
# been set in the JWT config
@pytest.mark.admin_secret
@pytest.mark.parametrize('endpoint', ['/v1/graphql', '/v1alpha1/graphql'])
class AbstractTestJWTClaimsMapWithStaticHasuraClaimsMapValues:
    def mk_claims(self, user_id=None):
        self.claims['https://myapp.com/jwt/claims'] = clean_null_terms({
            'user': {
                'id': user_id
            }
        })

    def test_jwt_claims_map_valid_claims_success(self, hge_ctx, jwt_configuration, endpoint):
        self.mk_claims('1')
        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['headers']['x-hasura-custom-header'] = 'custom-value'
        self.conf['url'] = endpoint
        self.conf['status'] = 200
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_claims_map_invalid_role_in_request_header(self, hge_ctx, jwt_configuration, endpoint):
        self.mk_claims('1')
        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
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

    def test_jwt_claims_map_claim_not_found(self, hge_ctx, jwt_configuration, endpoint):
        self.mk_claims(None)
        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
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
            self.conf = yaml.load(c)
        curr_time = datetime.now()
        exp_time = curr_time + timedelta(hours=1)
        self.claims = {
            'sub': '1234567890',
            'name': 'John Doe',
            'iat': math.floor(curr_time.timestamp()),
            'exp': math.floor(exp_time.timestamp())
        }

    @pytest.fixture(scope='class')
    def setup(self, postgis, hge_ctx):
        self.dir = 'queries/graphql_query/permissions'
        hge_ctx.v1q_f(self.dir + '/setup.yaml')
        yield
        hge_ctx.v1q_f(self.dir + '/teardown.yaml')


@pytest.mark.jwt('rsa', { 'claims_map': static_claims_map })
class TestJWTClaimsMapWithStaticHasuraClaimsMapValuesWithRSA(AbstractTestJWTClaimsMapWithStaticHasuraClaimsMapValues):
    pass


@pytest.mark.jwt('ed25519', { 'claims_map': static_claims_map })
class TestJWTClaimsMapWithStaticHasuraClaimsMapValuesWithEd25519(AbstractTestJWTClaimsMapWithStaticHasuraClaimsMapValues):
    pass
