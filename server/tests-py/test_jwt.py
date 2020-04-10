from datetime import datetime, timedelta
import math
import json
import time

import ruamel.yaml as yaml
import pytest
import jwt
from test_subscriptions import init_ws_conn
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.hazmat.primitives import serialization

from validate import check_query
from context import PytestConf


if not PytestConf.config.getoption('--hge-jwt-key-file'):
    pytest.skip('--hge-jwt-key-file is missing, skipping JWT tests', allow_module_level=True)

if not PytestConf.config.getoption('--hge-jwt-conf'):
    pytest.skip('--hge-jwt-key-conf is missing, skipping JWT tests', allow_module_level=True)

claims_namespace_path = PytestConf.config.getoption('--hge-jwt-claims-ns-path')

def get_claims_fmt(raw_conf):
    conf = json.loads(raw_conf)
    try:
        claims_fmt = conf['claims_format']
    except KeyError:
        claims_fmt = 'json'
    return claims_fmt

def mk_claims(conf, claims):
    claims_fmt = get_claims_fmt(conf)
    if claims_fmt == 'json':
        return claims
    elif claims_fmt == 'stringified_json':
        return json.dumps(claims)
    else:
        return claims

@pytest.mark.parametrize('endpoint', ['/v1/graphql', '/v1alpha1/graphql'])
class TestJWTBasic():

    def test_jwt_valid_claims_success(self, hge_ctx, endpoint):
        claims = mk_claims(hge_ctx.hge_jwt_conf, {
                'x-hasura-user-id': '1',
                'x-hasura-allowed-roles': ['user', 'editor'],
                'x-hasura-default-role': 'user'
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['url'] = endpoint
        self.conf['status'] = 200
        check_query(hge_ctx, self.conf, add_auth=False,claims_namespace_path=claims_namespace_path)

    def test_jwt_invalid_role_in_request_header(self, hge_ctx, endpoint):
        claims = mk_claims(hge_ctx.hge_jwt_conf, {
            'x-hasura-user-id': '1',
            'x-hasura-allowed-roles': ['contractor', 'editor'],
            'x-hasura-default-role': 'contractor'
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
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
        self.conf['url'] = endpoint
        if endpoint == '/v1/graphql':
            self.conf['status'] = 200
        if endpoint == '/v1alpha1/graphql':
            self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False,claims_namespace_path=claims_namespace_path)

    def test_jwt_no_allowed_roles_in_claim(self, hge_ctx, endpoint):
        claims = mk_claims(hge_ctx.hge_jwt_conf, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user'
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
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
        self.conf['url'] = endpoint
        if endpoint == '/v1/graphql':
            self.conf['status'] = 200
        if endpoint == '/v1alpha1/graphql':
            self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False,claims_namespace_path=claims_namespace_path)

    def test_jwt_invalid_allowed_roles_in_claim(self, hge_ctx, endpoint):
        claims = mk_claims(hge_ctx.hge_jwt_conf, {
            'x-hasura-user-id': '1',
            'x-hasura-allowed-roles': 'user',
            'x-hasura-default-role': 'user'
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
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
        self.conf['url'] = endpoint
        if endpoint == '/v1/graphql':
            self.conf['status'] = 200
        if endpoint == '/v1alpha1/graphql':
            self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False,claims_namespace_path=claims_namespace_path)

    def test_jwt_no_default_role(self, hge_ctx, endpoint):
        claims = mk_claims(hge_ctx.hge_jwt_conf, {
            'x-hasura-user-id': '1',
            'x-hasura-allowed-roles': ['user'],
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
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
        self.conf['url'] = endpoint
        if endpoint == '/v1/graphql':
            self.conf['status'] = 200
        if endpoint == '/v1alpha1/graphql':
            self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False,claims_namespace_path=claims_namespace_path)

    def test_jwt_expired(self, hge_ctx, endpoint):
        claims = mk_claims(hge_ctx.hge_jwt_conf, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims
        exp = datetime.utcnow() - timedelta(minutes=1)
        self.claims['exp'] = round(exp.timestamp())

        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['response'] = {
            'errors': [{
                'extensions': {
                    'code': 'invalid-jwt',
                    'path': '$'
                },
                'message': 'Could not verify JWT: JWTExpired'
            }]
        }
        self.conf['url'] = endpoint
        if endpoint == '/v1/graphql':
            self.conf['status'] = 200
        if endpoint == '/v1alpha1/graphql':
            self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False,claims_namespace_path=claims_namespace_path)

    def test_jwt_invalid_signature(self, hge_ctx, endpoint):
        claims = mk_claims(hge_ctx.hge_jwt_conf, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims
        wrong_key = gen_rsa_key()
        token = jwt.encode(self.claims, wrong_key, algorithm='HS256').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['response'] = {
            'errors': [{
                'extensions': {
                    'code': 'invalid-jwt',
                    'path': '$'
                },
                'message': 'Could not verify JWT: JWSError JWSInvalidSignature'
            }]
        }
        self.conf['url'] = endpoint
        if endpoint == '/v1/graphql':
            self.conf['status'] = 200
        if endpoint == '/v1alpha1/graphql':
            self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False,claims_namespace_path=claims_namespace_path)

    def test_jwt_no_audience_in_conf(self, hge_ctx, endpoint):
        jwt_conf = json.loads(hge_ctx.hge_jwt_conf)
        if 'audience' in jwt_conf:
            pytest.skip('audience present in conf, skipping testing no audience')

        claims = mk_claims(hge_ctx.hge_jwt_conf, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims
        self.claims['aud'] = 'hasura-test-suite'
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['url'] = endpoint
        check_query(hge_ctx, self.conf, add_auth=False,claims_namespace_path=claims_namespace_path)

    def test_jwt_no_issuer_in_conf(self, hge_ctx, endpoint):
        jwt_conf = json.loads(hge_ctx.hge_jwt_conf)
        if 'issuer' in jwt_conf:
            pytest.skip('issuer present in conf, skipping testing no issuer')

        claims = mk_claims(hge_ctx.hge_jwt_conf, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims
        self.claims['iss'] = 'rubbish-issuer'
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['url'] = endpoint
        check_query(hge_ctx, self.conf, add_auth=False,claims_namespace_path=claims_namespace_path)

    @pytest.fixture(autouse=True)
    def transact(self, setup):
        self.dir = 'queries/graphql_query/permissions'
        with open(self.dir + '/user_select_query_unpublished_articles.yaml') as c:
            self.conf = yaml.safe_load(c)
        curr_time = datetime.utcnow()
        exp_time = curr_time + timedelta(hours=10)
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


def gen_rsa_key():
    private_key = rsa.generate_private_key(
        public_exponent=65537,
        key_size=2048,
        backend=default_backend()
    )
    pem = private_key.private_bytes(
        encoding=serialization.Encoding.PEM,
        format=serialization.PrivateFormat.TraditionalOpenSSL,
        encryption_algorithm=serialization.NoEncryption()
    )
    return pem

class TestSubscriptionJwtExpiry(object):

    def test_jwt_expiry(self, hge_ctx, ws_client):
        curr_time = datetime.utcnow()
        self.claims = {
            'sub': '1234567890',
            'name': 'John Doe',
            'iat': math.floor(curr_time.timestamp())
        }
        claims = mk_claims(hge_ctx.hge_jwt_conf, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims
        exp = curr_time + timedelta(seconds=4)
        self.claims['exp'] = round(exp.timestamp())
        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        payload = {
            'headers': {
                'Authorization': 'Bearer ' + token
            }
        }
        init_ws_conn(hge_ctx, ws_client, payload)
        time.sleep(6)
        assert ws_client.remote_closed == True, ws_client.remote_closed


@pytest.mark.parametrize('endpoint', ['/v1/graphql', '/v1alpha1/graphql'])
class TestJwtAudienceCheck():
    def test_jwt_valid_audience(self, hge_ctx, endpoint):
        jwt_conf = json.loads(hge_ctx.hge_jwt_conf)
        if 'audience' not in jwt_conf:
            pytest.skip('audience not present in conf, skipping testing audience')

        audience = jwt_conf['audience']
        audience = audience if isinstance(audience, str) else audience[0]
        claims = mk_claims(hge_ctx.hge_jwt_conf, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims
        self.claims['aud'] = audience

        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        check_query(hge_ctx, self.conf, add_auth=False,claims_namespace_path=claims_namespace_path)

    def test_jwt_invalid_audience(self, hge_ctx, endpoint):
        jwt_conf = json.loads(hge_ctx.hge_jwt_conf)
        if 'audience' not in jwt_conf:
            pytest.skip('audience not present in conf, skipping testing audience')

        claims = mk_claims(hge_ctx.hge_jwt_conf, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims

        self.claims['aud'] = 'rubbish_audience'

        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['response'] = {
            'errors': [{
                'extensions': {
                    'code': 'invalid-jwt',
                    'path': '$'
                },
                'message': 'Could not verify JWT: JWTNotInAudience'
            }]
        }
        self.conf['url'] = endpoint
        if endpoint == '/v1/graphql':
            self.conf['status'] = 200
        if endpoint == '/v1alpha1/graphql':
            self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False,claims_namespace_path=claims_namespace_path)

    @pytest.fixture(autouse=True)
    def transact(self, setup):
        self.dir = 'queries/graphql_query/permissions'
        with open(self.dir + '/user_select_query_unpublished_articles.yaml') as c:
            self.conf = yaml.safe_load(c)
        curr_time = datetime.utcnow()
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

@pytest.mark.parametrize('endpoint', ['/v1/graphql', '/v1alpha1/graphql'])
class TestJwtIssuerCheck():
    def test_jwt_valid_issuer(self, hge_ctx, endpoint):
        jwt_conf = json.loads(hge_ctx.hge_jwt_conf)
        if 'issuer' not in jwt_conf:
            pytest.skip('issuer not present in conf, skipping testing issuer')

        issuer = jwt_conf['issuer']
        claims = mk_claims(hge_ctx.hge_jwt_conf, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims

        self.claims['iss'] = issuer

        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        check_query(hge_ctx, self.conf, add_auth=False,claims_namespace_path=claims_namespace_path)

    def test_jwt_invalid_issuer(self, hge_ctx, endpoint):
        jwt_conf = json.loads(hge_ctx.hge_jwt_conf)
        if 'issuer' not in jwt_conf:
            pytest.skip('issuer not present in conf, skipping testing issuer')

        claims = mk_claims(hge_ctx.hge_jwt_conf, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        if claims_namespace_path is None:
            self.claims['https://hasura.io/jwt/claims'] = claims
        elif claims_namespace_path == "$.hasuraClaims":
            self.claims['hasuraClaims'] = claims

        self.claims['iss'] = 'rubbish_issuer'

        token = jwt.encode(self.claims, hge_ctx.hge_jwt_key, algorithm='RS512').decode('utf-8')
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['response'] = {
            'errors': [{
                'extensions': {
                    'code': 'invalid-jwt',
                    'path': '$'
                },
                'message': 'Could not verify JWT: JWTNotInIssuer'
            }]
        }
        self.conf['url'] = endpoint
        if endpoint == '/v1/graphql':
            self.conf['status'] = 200
        if endpoint == '/v1alpha1/graphql':
            self.conf['status'] = 400
        check_query(hge_ctx, self.conf, add_auth=False,claims_namespace_path=claims_namespace_path)

    @pytest.fixture(autouse=True)
    def transact(self, setup):
        self.dir = 'queries/graphql_query/permissions'
        with open(self.dir + '/user_select_query_unpublished_articles.yaml') as c:
            self.conf = yaml.safe_load(c)
        curr_time = datetime.utcnow()
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
