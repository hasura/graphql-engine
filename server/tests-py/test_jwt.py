from datetime import datetime, timedelta, timezone
import math
import json
import time

import pytest
import jwt
from test_subscriptions import init_ws_conn
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.hazmat.primitives import serialization
from ruamel.yaml import YAML

from validate import check_query


yaml=YAML(typ='safe', pure=True)


def format_claims(conf, claims):
    match conf.get('claims_format', 'json'):
        case 'stringified_json':
            return json.dumps(claims)
        case _:
            return claims


def set_claims(claims, hasura_claims, namespace_path=None):
    match namespace_path:
        case None:
            claims['https://hasura.io/jwt/claims'] = hasura_claims
        case "$":
            claims.update(hasura_claims)
        case "$.hasura_claims":
            claims['hasura_claims'] = hasura_claims
        case "$.hasura.claims":
            claims['hasura'] = {}
            claims['hasura']['claims'] = hasura_claims
        case "$.hasura['claims%']":
            claims['hasura'] = {}
            claims['hasura']['claims%'] = hasura_claims
        case _:
            raise Exception(
                    '''claims_namespace_path should not be anything
                    other than $.hasura_claims, $.hasura['claims%'] or $ for testing. The
                    value of claims_namespace_path was {}'''.format(namespace_path))
    return claims


def get_header_fmt(conf):
    try:
        hdr_fmt = conf['header']['type']
        if hdr_fmt == 'Authorization':
            return (hdr_fmt, None)
        elif hdr_fmt == 'Cookie':
            return (hdr_fmt, conf['header']['name'])
        elif hdr_fmt == "CustomHeader":
            return (hdr_fmt, conf['header']['name'])
        else:
            raise Exception('Invalid JWT header format: %s' % conf)
    except KeyError:
        print('header conf not found in JWT conf, defaulting to Authorization')
        hdr_fmt = ('Authorization', None)
        return hdr_fmt


def mk_authz_header(conf, token):
    (header, name) = get_header_fmt(conf)
    if header == 'Authorization':
        return {'Authorization': 'Bearer ' + token}
    elif header == 'Cookie' and name:
        return {'Cookie': name + '=' + token}
    elif header == 'CustomHeader' and name:
        return {name: token}
    else:
        raise Exception('Invalid JWT header format')


# The tests below would ideally use parameterization rather than subclassing,
# but that doesn't work because of `hge_fixture_env`, which creates a "soft"
# dependency between the environment variables and `hge_server`. Parameterizing
# the former *should* force the latter to be recreated for each new set of
# environment variables, but `hge_server` isn't actually aware there's a
# dependency. See `TestParameterizedFixtures` in test_tests.py for more
# information.


@pytest.mark.admin_secret
@pytest.mark.parametrize('endpoint', ['/v1/graphql', '/v1alpha1/graphql'])
class AbstractTestJwtBasic:
    def test_jwt_valid_claims_success(self, hge_ctx, jwt_configuration, endpoint):
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
                'x-hasura-user-id': '1',
                'x-hasura-allowed-roles': ['user', 'editor'],
                'x-hasura-default-role': 'user'
        })
        claims_namespace_path = jwt_configuration.server_configuration.get('claims_namespace_path')
        self.claims = set_claims(self.claims, hasura_claims, claims_namespace_path)

        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        authz_header = mk_authz_header(jwt_configuration.server_configuration, token)
        self.conf['headers'].update(authz_header)
        self.conf['url'] = endpoint
        self.conf['status'] = 200
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_invalid_role_in_request_header(self, hge_ctx, jwt_configuration, endpoint):
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-allowed-roles': ['contractor', 'editor'],
            'x-hasura-default-role': 'contractor'
        })
        claims_namespace_path = jwt_configuration.server_configuration.get('claims_namespace_path')
        self.claims = set_claims(self.claims, hasura_claims, claims_namespace_path)

        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        authz_header = mk_authz_header(jwt_configuration.server_configuration, token)
        self.conf['headers'].update(authz_header)
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

    def test_jwt_no_allowed_roles_in_claim(self, hge_ctx, jwt_configuration, endpoint):
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user'
        })
        claims_namespace_path = jwt_configuration.server_configuration.get('claims_namespace_path')
        self.claims = set_claims(self.claims, hasura_claims, claims_namespace_path)

        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        authz_header = mk_authz_header(jwt_configuration.server_configuration, token)
        self.conf['headers'].update(authz_header)
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
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_invalid_allowed_roles_in_claim(self, hge_ctx, jwt_configuration, endpoint):
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-allowed-roles': 'user',
            'x-hasura-default-role': 'user'
        })
        claims_namespace_path = jwt_configuration.server_configuration.get('claims_namespace_path')
        self.claims = set_claims(self.claims, hasura_claims, claims_namespace_path)

        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        authz_header = mk_authz_header(jwt_configuration.server_configuration, token)
        self.conf['headers'].update(authz_header)
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

    def test_jwt_no_default_role(self, hge_ctx, jwt_configuration, endpoint):
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-allowed-roles': ['user'],
        })
        claims_namespace_path = jwt_configuration.server_configuration.get('claims_namespace_path')
        self.claims = set_claims(self.claims, hasura_claims, claims_namespace_path)

        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        authz_header = mk_authz_header(jwt_configuration.server_configuration, token)
        self.conf['headers'].update(authz_header)
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
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_expired(self, hge_ctx, jwt_configuration, endpoint):
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        claims_namespace_path = jwt_configuration.server_configuration.get('claims_namespace_path')
        self.claims = set_claims(self.claims, hasura_claims, claims_namespace_path)
        exp = datetime.now() - timedelta(minutes=1)
        self.claims['exp'] = round(exp.timestamp())

        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        authz_header = mk_authz_header(jwt_configuration.server_configuration, token)
        self.conf['headers'].update(authz_header)
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
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_invalid_signature(self, hge_ctx, jwt_configuration, endpoint):
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        claims_namespace_path = jwt_configuration.server_configuration.get('claims_namespace_path')
        self.claims = set_claims(self.claims, hasura_claims, claims_namespace_path)

        wrong_key = str(self.gen_rsa_key())
        other_algo = 'HS256'
        if jwt_configuration.algorithm == 'HS256':
            other_algo = 'HS384'
        token = jwt.encode(self.claims, wrong_key, algorithm=other_algo)
        authz_header = mk_authz_header(jwt_configuration.server_configuration, token)
        self.conf['headers'].update(authz_header)
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
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_no_audience_in_conf(self, hge_ctx, jwt_configuration, endpoint):
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        claims_namespace_path = jwt_configuration.server_configuration.get('claims_namespace_path')
        self.claims = set_claims(self.claims, hasura_claims, claims_namespace_path)
        self.claims['aud'] = 'hasura-test-suite'

        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        authz_header = mk_authz_header(jwt_configuration.server_configuration, token)
        self.conf['headers'].update(authz_header)
        self.conf['url'] = endpoint
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_no_issuer_in_conf(self, hge_ctx, jwt_configuration, endpoint):
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        claims_namespace_path = jwt_configuration.server_configuration.get('claims_namespace_path')
        self.claims = set_claims(self.claims, hasura_claims, claims_namespace_path)
        self.claims['iss'] = 'rubbish-issuer'

        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        authz_header = mk_authz_header(jwt_configuration.server_configuration, token)
        self.conf['headers'].update(authz_header)
        self.conf['url'] = endpoint
        check_query(hge_ctx, self.conf, add_auth=False)

    @pytest.fixture(autouse=True)
    def transact(self, setup):
        self.dir = 'queries/graphql_query/permissions'
        with open(self.dir + '/user_select_query_unpublished_articles.yaml') as c:
            self.conf = yaml.load(c)
        curr_time = datetime.now()
        exp_time = curr_time + timedelta(hours=10)
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

    @staticmethod
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


@pytest.mark.jwt('rsa')
class TestJwtBasicWithRsa(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('ed25519')
class TestJwtBasicWithEd25519(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('es')
class TestJwtBasicWithEs(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('rsa', {
    'header': {'type': 'Cookie', 'name': 'hasura_user'},
})
class TestJwtBasicWithRsaAndCookie(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('ed25519', {
    'header': {'type': 'Cookie', 'name': 'hasura_user'},
})
class TestJwtBasicWithEd25519AndCookie(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('es', {
    'header': {'type': 'Cookie', 'name': 'hasura_user'},
})
class TestJwtBasicWithEsAndCookie(AbstractTestJwtBasic):
    pass

@pytest.mark.jwt('rsa', {
    'header': {'type': 'CustomHeader', 'name': 'hasura_user'},
})
class TestJwtBasicWithRsaAndCustomHeader(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('rsa', {
    'claims_format': 'stringified_json',
})
class TestJwtBasicWithRsaAndStringifiedJsonClaims(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('ed25519', {
    'claims_format': 'stringified_json',
})
class TestJwtBasicWithEd25519AndStringifiedJsonClaims(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('es', {
    'claims_format': 'stringified_json',
})
class TestJwtBasicWithEsAndStringifiedJsonClaims(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('rsa', {
    'claims_namespace_path': '$',
})
class TestJwtBasicWithRsaAndClaimsNamespacePathAtRoot(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('ed25519', {
    'claims_namespace_path': '$',
})
class TestJwtBasicWithEd25519AndClaimsNamespacePathAtRoot(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('es', {
    'claims_namespace_path': '$',
})
class TestJwtBasicWithEsAndClaimsNamespacePathAtRoot(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('rsa', {
    'claims_namespace_path': '$.hasura_claims',
})
class TestJwtBasicWithRsaAndClaimsNamespacePathAtOneLevelOfNesting(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('ed25519', {
    'claims_namespace_path': '$.hasura_claims',
})
class TestJwtBasicWithEd25519AndClaimsNamespacePathAtOneLevelOfNesting(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('es', {
    'claims_namespace_path': '$.hasura_claims',
})
class TestJwtBasicWithEsAndClaimsNamespacePathAtOneLevelOfNesting(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('rsa', {
    'claims_namespace_path': '$.hasura.claims',
})
class TestJwtBasicWithRsaAndClaimsNamespacePathAtTwoLevelsOfNesting(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('ed25519', {
    'claims_namespace_path': '$.hasura.claims',
})
class TestJwtBasicWithEd25519AndClaimsNamespacePathAtTwoLevelsOfNesting(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('es', {
    'claims_namespace_path': '$.hasura.claims',
})
class TestJwtBasicWithEsAndClaimsNamespacePathAtTwoLevelsOfNesting(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('rsa', {
    'claims_namespace_path': '$.hasura[\'claims%\']',
})
class TestJwtBasicWithRsaAndClaimsNamespacePathWithSpecialCharacters(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('ed25519', {
    'claims_namespace_path': '$.hasura[\'claims%\']',
})
class TestJwtBasicWithEd25519AndClaimsNamespacePathWithSpecialCharacters(AbstractTestJwtBasic):
    pass


@pytest.mark.jwt('es', {
    'claims_namespace_path': '$.hasura[\'claims%\']',
})
class TestJwtBasicWithEsAndClaimsNamespacePathWithSpecialCharacters(AbstractTestJwtBasic):
    pass


@pytest.mark.admin_secret
@pytest.mark.parametrize('endpoint', ['/v1/graphql', '/v1alpha1/graphql'])
class AbstractTestJwtExpirySkew:

    def test_jwt_expiry_leeway(self, hge_ctx, jwt_configuration, endpoint):
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        self.claims = set_claims(self.claims, hasura_claims)
        exp = datetime.now(timezone.utc) - timedelta(seconds = 30)
        self.claims['exp'] = round(exp.timestamp())
        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        self.conf['response'] = {
            'data': {
                'article': [{
                    'id': 1,
                    'title': 'Article 1',
                    'content': 'Sample article content 1',
                    'is_published': False,
                    'author': {
                        'id': 1,
                        'name': 'Author 1'
                    }
                }]
            }
        }
        self.conf['url'] = endpoint
        self.conf['status'] = 200
        check_query(hge_ctx, self.conf, add_auth=False)

    @pytest.fixture(autouse=True)
    def transact(self, setup):
        self.dir = 'queries/graphql_query/permissions'
        with open(self.dir + '/user_select_query_unpublished_articles.yaml') as c:
            self.conf = yaml.load(c)
        curr_time = datetime.now()
        exp_time = curr_time + timedelta(hours=10)
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


@pytest.mark.jwt('rsa', {
    'allowed_skew': 60,
})
class TestJwtExpirySkewWithRsa(AbstractTestJwtExpirySkew):
    pass


@pytest.mark.jwt('ed25519', {
    'allowed_skew': 60,
})
class TestJwtExpirySkewWithEd25519(AbstractTestJwtExpirySkew):
    pass


@pytest.mark.jwt('es', {
    'allowed_skew': 60,
})
class TestJwtExpirySkewWithEs(AbstractTestJwtExpirySkew):
    pass


@pytest.mark.admin_secret
class AbstractTestSubscriptionJwtExpiry:
    def test_jwt_expiry(self, hge_key, jwt_configuration, ws_client):
        curr_time = datetime.now()
        self.claims = {
            'sub': '1234567890',
            'name': 'John Doe',
            'iat': math.floor(curr_time.timestamp())
        }
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        self.claims = set_claims(self.claims, hasura_claims)
        exp = curr_time + timedelta(seconds=4)
        self.claims['exp'] = round(exp.timestamp())

        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        authz_header = mk_authz_header(jwt_configuration.server_configuration, token)
        payload = dict()
        payload['headers'] = authz_header
        init_ws_conn(hge_key, ws_client, payload)
        time.sleep(6)
        assert ws_client.remote_closed == True, ws_client.remote_closed


@pytest.mark.jwt('rsa')
class TestSubscriptionJwtExpiryWithRsa(AbstractTestSubscriptionJwtExpiry):
    pass


@pytest.mark.jwt('ed25519')
class TestSubscriptionJwtExpiryWithEd25519(AbstractTestSubscriptionJwtExpiry):
    pass


@pytest.mark.jwt('es')
class TestSubscriptionJwtExpiryWithEs(AbstractTestSubscriptionJwtExpiry):
    pass


@pytest.mark.admin_secret
@pytest.mark.parametrize('endpoint', ['/v1/graphql', '/v1alpha1/graphql'])
class AbstractTestJwtAudienceCheck:
    def test_jwt_valid_audience(self, hge_ctx, jwt_configuration, endpoint):
        audience = jwt_configuration.server_configuration['audience']
        audience = audience if isinstance(audience, str) else audience[0]
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        self.claims = set_claims(self.claims, hasura_claims)
        self.claims['aud'] = audience
        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_invalid_audience(self, hge_ctx, jwt_configuration, endpoint):
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        self.claims = set_claims(self.claims, hasura_claims)
        self.claims['aud'] = 'rubbish_audience'

        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
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


@pytest.mark.jwt('rsa', {
    'audience': 'myapp-1234',
})
class TestJwtAudienceCheckWithRsaAndSingleAudience(AbstractTestJwtAudienceCheck):
    pass


@pytest.mark.jwt('ed25519', {
    'audience': 'myapp-1234',
})
class TestJwtAudienceCheckWithEd25519AndSingleAudience(AbstractTestJwtAudienceCheck):
    pass


@pytest.mark.jwt('es', {
    'audience': 'myapp-1234',
})
class TestJwtAudienceCheckWithEsAndSingleAudience(AbstractTestJwtAudienceCheck):
    pass


@pytest.mark.jwt('rsa', {
    'audience': ['myapp-1234', 'myapp-9876'],
})
class TestJwtAudienceCheckWithRsaAndListOfAudiences(AbstractTestJwtAudienceCheck):
    pass


@pytest.mark.jwt('ed25519', {
    'audience': ['myapp-1234', 'myapp-9876'],
})
class TestJwtAudienceCheckWithEd25519AndListOfAudiences(AbstractTestJwtAudienceCheck):
    pass


@pytest.mark.jwt('es', {
    'audience': ['myapp-1234', 'myapp-9876'],
})
class TestJwtAudienceCheckWithEsAndListOfAudiences(AbstractTestJwtAudienceCheck):
    pass


@pytest.mark.admin_secret
@pytest.mark.parametrize('endpoint', ['/v1/graphql', '/v1alpha1/graphql'])
class AbstractTestJwtIssuerCheck:
    def test_jwt_valid_issuer(self, hge_ctx, jwt_configuration, endpoint):
        issuer = jwt_configuration.server_configuration['issuer']
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        self.claims = set_claims(self.claims, hasura_claims)
        self.claims['iss'] = issuer

        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
        self.conf['headers']['Authorization'] = 'Bearer ' + token
        check_query(hge_ctx, self.conf, add_auth=False)

    def test_jwt_invalid_issuer(self, hge_ctx, jwt_configuration, endpoint):
        hasura_claims = format_claims(jwt_configuration.server_configuration, {
            'x-hasura-user-id': '1',
            'x-hasura-default-role': 'user',
            'x-hasura-allowed-roles': ['user'],
        })
        self.claims = set_claims(self.claims, hasura_claims)
        self.claims['iss'] = 'rubbish_issuer'

        token = jwt.encode(self.claims, jwt_configuration.private_key, algorithm=jwt_configuration.algorithm)
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


@pytest.mark.jwt('rsa', {
    'issuer': 'https://hasura.com',
})
class TestJwtIssuerCheckWithRsa(AbstractTestJwtIssuerCheck):
    pass


@pytest.mark.jwt('ed25519', {
    'issuer': 'https://hasura.com',
})
class TestJwtIssuerCheckWithEd25519(AbstractTestJwtIssuerCheck):
    pass


@pytest.mark.jwt('es', {
    'issuer': 'https://hasura.com',
})
class TestJwtIssuerCheckWithEs(AbstractTestJwtIssuerCheck):
    pass
