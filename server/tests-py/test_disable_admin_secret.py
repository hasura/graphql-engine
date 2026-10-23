import json
import os
import secrets
import subprocess
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

import jwt
import requests
from cryptography.hazmat.primitives.asymmetric import rsa

import pytest

import ports


def test_disable_admin_secret_requires_fallback_auth(
    hge_bin, metadata_schema_url, worker_id
):
    if not hge_bin:
        pytest.skip('This test requires --hge-bin')

    port = ports.find_free_port(worker_id)
    env = {
        **os.environ,
        'HASURA_GRAPHQL_DISABLE_ADMIN_SECRET': 'true',
    }
    for name in (
        'HASURA_GRAPHQL_AUTH_HOOK',
        'HASURA_GRAPHQL_JWT_SECRET',
        'HASURA_GRAPHQL_JWT_SECRETS',
        'HASURA_GRAPHQL_SSO_PROVIDERS',
    ):
        env.pop(name, None)

    result = subprocess.run(
        [
            hge_bin,
            '--metadata-database-url',
            metadata_schema_url,
            'serve',
            '--server-port',
            str(port),
            '--disable-admin-secret',
        ],
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        encoding='utf-8',
        timeout=30,
    )

    assert result.returncode != 0
    assert (
        'Fatal Error: requires either HASURA_GRAPHQL_AUTH_HOOK'
    ) in result.stdout


@pytest.mark.parametrize('disable_admin_secret', [False, True])
@pytest.mark.parametrize('configuration', ['jwks', 'mixed', 'static'])
def test_jwk_loading_and_rotation(
    hge_bin, metadata_schema_url, worker_id, tmp_path, request,
    disable_admin_secret, configuration,
):
    """JWT-only auth must populate and refresh JWKs, without restoring admin auth."""
    if not hge_bin:
        pytest.skip('This test requires --hge-bin')

    if configuration == 'mixed' and not request.config.getoption('--pro-tests'):
        pytest.skip('Multiple JWT configurations require --pro-tests')

    keys = {
        kid: rsa.generate_private_key(public_exponent=65537, key_size=2048)
        for kid in ('initial', 'rotated')
    }
    public_keys = {}
    for kid, key in keys.items():
        public_keys[kid] = json.loads(jwt.algorithms.RSAAlgorithm.to_jwk(key.public_key()))
        public_keys[kid].update(kid=kid, use='sig', alg='RS256')
    state = {'kid': 'initial', 'requests': 0}

    class JwkHandler(BaseHTTPRequestHandler):
        def do_GET(self):
            body = json.dumps({'keys': [public_keys[state['kid']]]}).encode()
            self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.send_header('Cache-Control', 'max-age=1')
            self.send_header('Content-Length', str(len(body)))
            self.end_headers()
            self.wfile.write(body)
            state['requests'] += 1

        def log_message(self, *args):
            pass

    jwks = ThreadingHTTPServer(('127.0.0.1', 0), JwkHandler)
    server_thread = threading.Thread(target=jwks.serve_forever, daemon=True)
    server_thread.start()
    admin_secret = secrets.token_urlsafe(32)
    static_key = secrets.token_urlsafe(48)
    jwt_configs = []
    if configuration != 'static':
        jwt_configs.append({
            'jwk_url': f'http://127.0.0.1:{jwks.server_port}/jwks',
            'issuer': 'https://jwt-test.invalid/rsa',
            'audience': 'jwt-test',
        })
    if configuration != 'jwks':
        jwt_configs.append({
            'type': 'HS256', 'key': static_key,
            'issuer': 'https://jwt-test.invalid/hs',
            'audience': 'jwt-test',
        })

    def token(kid='initial', static=False, **overrides):
        now = int(time.time())
        claims = {
            'iss': 'https://jwt-test.invalid/hs' if static else 'https://jwt-test.invalid/rsa',
            'aud': 'jwt-test', 'iat': now, 'exp': now + 600,
            'https://hasura.io/jwt/claims': {
                'x-hasura-default-role': 'user',
                'x-hasura-allowed-roles': ['user'],
            },
            **overrides,
        }
        return jwt.encode(
            claims, static_key if static else keys[kid],
            algorithm='HS256' if static else 'RS256',
            headers={} if static else {'kid': kid},
        )

    port = ports.find_free_port(worker_id)
    url = f'http://127.0.0.1:{port}'
    env = {
        **os.environ,
        'HASURA_GRAPHQL_ADMIN_SECRET': admin_secret,
        'HASURA_GRAPHQL_DISABLE_ADMIN_SECRET': str(disable_admin_secret).lower(),
    }
    for name in ('HASURA_GRAPHQL_AUTH_HOOK', 'HASURA_GRAPHQL_JWT_SECRET',
                 'HASURA_GRAPHQL_JWT_SECRETS', 'HASURA_GRAPHQL_ADMIN_SECRETS',
                 'HASURA_GRAPHQL_SSO_PROVIDERS', 'HASURA_GRAPHQL_UNAUTHORIZED_ROLE'):
        env.pop(name, None)
    # The singular option is supported by both OSS and Pro. The plural option
    # is Pro-only, so use it only for the mixed-configuration regression.
    if configuration == 'mixed':
        env['HASURA_GRAPHQL_JWT_SECRETS'] = json.dumps(jwt_configs)
    else:
        env['HASURA_GRAPHQL_JWT_SECRET'] = json.dumps(jwt_configs[0])

    def query(signed_token=None, admin=False):
        headers = {}
        if signed_token is not None:
            headers['Authorization'] = 'Bearer ' + signed_token
        if admin:
            headers['x-hasura-admin-secret'] = admin_secret
        return requests.post(
            url + '/v1/graphql', json={'query': '{ __typename }'},
            headers=headers, timeout=5,
        ).json()

    expected = {'data': {'__typename': 'query_root'}}
    process = None
    # Logs can contain generated configuration. Keep them local to the test.
    with (tmp_path / 'hge.log').open('w') as log:
        try:
            process = subprocess.Popen(
                [hge_bin, '--metadata-database-url', metadata_schema_url,
                 'serve', '--server-port', str(port), '--enable-telemetry=false'],
                env=env, stdout=log, stderr=subprocess.STDOUT,
            )
            deadline = time.monotonic() + 30
            while time.monotonic() < deadline:
                assert process.poll() is None, 'Engine exited before becoming healthy'
                try:
                    if requests.get(url + '/healthz', timeout=1).status_code == 200:
                        break
                except requests.RequestException:
                    pass
                time.sleep(0.2)
            else:
                pytest.fail('Engine did not become healthy within 30 seconds')

            if configuration != 'jwks':
                assert query(token(static=True)) == expected
                assert 'errors' in query(token(static=True, aud='wrong-audience'))
                assert 'errors' in query(token(static=True, iss='https://jwt-test.invalid/wrong'))
                assert 'errors' in query(token(static=True, exp=int(time.time()) - 60))
            if configuration != 'static':
                initial_token = token()
                # The initial poll can race with HTTP readiness.
                deadline = time.monotonic() + 5
                response = query(initial_token)
                while response != expected and time.monotonic() < deadline:
                    time.sleep(0.1)
                    response = query(initial_token)
                assert response == expected, response
                assert 'errors' in query(token(aud='wrong-audience'))
                assert 'errors' in query(token(iss='https://jwt-test.invalid/wrong'))
                assert 'errors' in query(token(exp=int(time.time()) - 60))

                # HGE polls at 60-second intervals, independent of max-age=1.
                before_rotation = state['requests']
                state['kid'] = 'rotated'
                rotated_token = token(kid='rotated')
                deadline = time.monotonic() + 75
                while query(rotated_token) != expected:
                    assert time.monotonic() < deadline, 'Rotated JWK was not loaded'
                    time.sleep(0.5)
                assert state['requests'] > before_rotation
                assert 'errors' in query(initial_token)
                if configuration == 'mixed':
                    assert query(token(static=True)) == expected
            else:
                assert state['requests'] == 0, 'Static JWT must not fetch JWKs'

            assert 'errors' in query('synthetic.invalid.token')
            assert 'errors' in query()
            if disable_admin_secret:
                assert 'errors' in query(admin=True)
            else:
                assert query(admin=True) == expected
        finally:
            if process is not None:
                process.terminate()
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait(timeout=5)
            jwks.shutdown()
            jwks.server_close()
            server_thread.join(timeout=5)
