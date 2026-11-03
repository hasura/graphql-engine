"""
End-to-end tests for the authentication scenario matrix.

Each scenario is a combination of the auth-related server options (admin
secret, auth hook, JWT, unauthorized role and HASURA_GRAPHQL_DISABLE_ADMIN_SECRET)
and is identified by the same ID as in `authScenarioMatrixTests` in
server/src-test/Hasura/Server/AuthSpec.hs:

* A1-A7: admin-secret authentication enabled.
* B1-B10: admin-secret authentication disabled.

For every configuration that starts, the engine is run against a shared
metadata database and probed with the same set of requests. The resolved role
is observed through a `whoami` table whose rows are only visible to the role
of the same name (admin sees every row), and admin-only API access is checked
through `export_metadata`. Configurations that must not start are checked for
a non-zero exit and the expected error message.
"""

import contextlib
import json
import os
import secrets
import subprocess
import threading
import time
from dataclasses import dataclass
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from typing import Iterator, Optional

import jwt
import pytest
import requests
import websocket

import fixtures.postgres
import ports

_PASS_THROUGH_ENV_VARS = ('PATH', 'ODBCSYSINI', 'ODBCINSTINI')

ADMIN_SECRET = 'auth-scenarios-' + secrets.token_urlsafe(16)
JWT_KEY = secrets.token_urlsafe(48)
JWT_CONFIG = json.dumps({'type': 'HS256', 'key': JWT_KEY})
WHOAMI_ROLES = ['anon', 'hook', 'r00t', 'user']
WHOAMI_QUERY = '{ whoami(order_by: {role: asc}) { role } }'

# Request outcomes: a role name, or 'E:<error code>'.
ACCESS_DENIED = 'E:access-denied'
INVALID_HEADERS = 'E:invalid-headers'
INVALID_JWT = 'E:invalid-jwt'


@dataclass(frozen=True)
class Scenario:
    id: str
    secret: bool = False
    hook: bool = False
    jwt: bool = False
    unauthorized_role: Optional[str] = None
    disabled: bool = False

    def __str__(self) -> str:
        flags = (
            (['admin secret'] if self.secret else [])
            + (['auth hook'] if self.hook else [])
            + (['JWT'] if self.jwt else [])
            + ([f'unauthorized role {self.unauthorized_role}'] if self.unauthorized_role else [])
            + (['admin secret disabled'] if self.disabled else [])
        )
        return f'{self.id}: {" + ".join(flags) or "nothing set"}'


def _valid_jwt(**overrides) -> str:
    now = int(time.time())
    claims = {
        'sub': '1',
        'iat': now,
        'exp': now + 3600,
        'https://hasura.io/jwt/claims': {
            'x-hasura-allowed-roles': ['user'],
            'x-hasura-default-role': 'user',
        },
        **overrides,
    }
    return jwt.encode(claims, JWT_KEY, algorithm='HS256')


# The requests sent to every configuration that starts, in the column order of
# REQUEST_MATRIX. JWTs are minted per call so that they never expire.
REQUESTS = [
    ('no auth headers', lambda: {}),
    ('correct admin secret', lambda: {'x-hasura-admin-secret': ADMIN_SECRET}),
    ('wrong admin secret', lambda: {'x-hasura-admin-secret': 'wrong'}),
    ('x-hasura-role: admin', lambda: {'x-hasura-role': 'admin'}),
    ('correct admin secret + x-hasura-role: r00t', lambda: {'x-hasura-admin-secret': ADMIN_SECRET, 'x-hasura-role': 'r00t'}),
    ('valid JWT', lambda: {'Authorization': 'Bearer ' + _valid_jwt()}),
    ('valid JWT + x-hasura-role: admin', lambda: {'Authorization': 'Bearer ' + _valid_jwt(), 'x-hasura-role': 'admin'}),
    ('invalid JWT', lambda: {'Authorization': 'Bearer not.a.jwt'}),
]

# Expected outcome of each request in REQUESTS, per configuration that starts.
REQUEST_MATRIX = [
    (Scenario('A1'),
     ['admin', 'admin', 'admin', 'admin', 'r00t', 'admin', 'admin', 'admin']),
    (Scenario('A3', secret=True),
     [ACCESS_DENIED, 'admin', ACCESS_DENIED, ACCESS_DENIED, 'r00t', ACCESS_DENIED, ACCESS_DENIED, ACCESS_DENIED]),
    (Scenario('A4', secret=True, unauthorized_role='anon'),
     ['anon', 'admin', ACCESS_DENIED, 'anon', 'r00t', 'anon', 'anon', 'anon']),
    (Scenario('A5', secret=True, jwt=True, unauthorized_role='anon'),
     ['anon', 'admin', ACCESS_DENIED, 'anon', 'r00t', 'user', ACCESS_DENIED, INVALID_JWT]),
    (Scenario('A6', secret=True, hook=True),
     ['hook', 'admin', ACCESS_DENIED, 'hook', 'r00t', 'hook', 'hook', 'hook']),
    (Scenario('B2', disabled=True, unauthorized_role='anon'),
     ['anon'] * len(REQUESTS)),
    (Scenario('B2', secret=True, disabled=True, unauthorized_role='anon'),
     ['anon'] * len(REQUESTS)),
    (Scenario('B4', secret=True, disabled=True, jwt=True),
     [INVALID_HEADERS, INVALID_HEADERS, INVALID_HEADERS, INVALID_HEADERS, INVALID_HEADERS, 'user', ACCESS_DENIED, INVALID_JWT]),
    (Scenario('B5', secret=True, disabled=True, jwt=True, unauthorized_role='anon'),
     ['anon', 'anon', 'anon', 'anon', 'anon', 'user', ACCESS_DENIED, INVALID_JWT]),
    (Scenario('B7', secret=True, disabled=True, hook=True),
     ['hook'] * len(REQUESTS)),
    (Scenario('B8', secret=True, disabled=True, hook=True, unauthorized_role='anon'),
     ['hook'] * len(REQUESTS)),
]

ADMIN_ROLE_REJECTED = ['HASURA_GRAPHQL_UNAUTHORIZED_ROLE', 'cannot be', '--disable-admin-secret']
WEBHOOK_AND_JWT = ['Both webhook and JWT mode cannot be enabled']

# Configurations that must refuse to start, with substrings of the error.
STARTUP_FAILURES = [
    (Scenario('A2', unauthorized_role='anon'),
     ['HASURA_GRAPHQL_UNAUTHORIZED_ROLE) requires --admin-secret']),
    (Scenario('A6', secret=True, hook=True, unauthorized_role='anon'),
     ['is not allowed when --auth-hook']),
    (Scenario('A7', jwt=True), ['HASURA_GRAPHQL_JWT_SECRET) requires --admin-secret']),
    (Scenario('A7', hook=True), ['HASURA_GRAPHQL_AUTH_HOOK) requires --admin-secret']),
    (Scenario('A7', secret=True, hook=True, jwt=True), WEBHOOK_AND_JWT),
    (Scenario('B1', disabled=True), ['requires either HASURA_GRAPHQL_AUTH_HOOK']),
    (Scenario('B1', secret=True, disabled=True), ['requires either HASURA_GRAPHQL_AUTH_HOOK']),
    (Scenario('B3', secret=True, disabled=True, unauthorized_role='admin'), ADMIN_ROLE_REJECTED),
    (Scenario('B6', disabled=True, jwt=True, unauthorized_role='admin'), ADMIN_ROLE_REJECTED),
    (Scenario('B9', disabled=True, hook=True, unauthorized_role='admin'), ADMIN_ROLE_REJECTED),
    (Scenario('B10', disabled=True, hook=True, jwt=True), WEBHOOK_AND_JWT),
]


@dataclass
class AuthScenariosContext:
    hge_bin: str
    metadata_url: str
    worker_id: str
    hook_url: str
    log_dir: Path

    def env(self, scenario: Scenario) -> dict[str, str]:
        env = {name: os.environ[name] for name in _PASS_THROUGH_ENV_VARS if name in os.environ}
        if scenario.secret:
            env['HASURA_GRAPHQL_ADMIN_SECRET'] = ADMIN_SECRET
        if scenario.hook:
            env['HASURA_GRAPHQL_AUTH_HOOK'] = self.hook_url
        if scenario.jwt:
            env['HASURA_GRAPHQL_JWT_SECRET'] = JWT_CONFIG
        if scenario.unauthorized_role:
            env['HASURA_GRAPHQL_UNAUTHORIZED_ROLE'] = scenario.unauthorized_role
        if scenario.disabled:
            env['HASURA_GRAPHQL_DISABLE_ADMIN_SECRET'] = 'true'
        return env

    def args(self, port: int) -> list[str]:
        return [
            self.hge_bin,
            '--metadata-database-url', self.metadata_url,
            'serve',
            '--server-port', str(port),
            '--enable-telemetry=false',
        ]

    @contextlib.contextmanager
    def running(self, scenario: Scenario) -> Iterator[str]:
        port = ports.find_free_port(self.worker_id)
        url = f'http://127.0.0.1:{port}'
        log_path = self.log_dir / f'{scenario.id}-{port}.log'
        with log_path.open('w') as log:
            process = subprocess.Popen(
                self.args(port), env=self.env(scenario),
                stdout=log, stderr=subprocess.STDOUT,
            )
            try:
                deadline = time.monotonic() + 60
                while True:
                    if process.poll() is not None:
                        pytest.fail(f'{scenario} exited at startup:\n{log_path.read_text()}')
                    try:
                        if requests.get(url + '/healthz', timeout=1).status_code == 200:
                            break
                    except requests.RequestException:
                        pass
                    if time.monotonic() > deadline:
                        pytest.fail(f'{scenario} did not become healthy:\n{log_path.read_text()}')
                    time.sleep(0.2)
                yield url
            finally:
                process.terminate()
                try:
                    process.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait(timeout=10)


class _HookHandler(BaseHTTPRequestHandler):
    """An auth webhook that authenticates every request as role `hook`."""

    def _respond(self):
        body = json.dumps({'X-Hasura-Role': 'hook'}).encode()
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    do_GET = _respond

    def do_POST(self):
        self.rfile.read(int(self.headers.get('Content-Length') or 0))
        self._respond()

    def log_message(self, *args):
        pass


def _admin_request(url: str, path: str, body: dict) -> dict:
    response = requests.post(url + path, json=body, headers={'x-hasura-admin-secret': ADMIN_SECRET}, timeout=60)
    assert response.status_code == 200, response.text
    return response.json()


def _setup_metadata(url: str, source_url: str):
    _admin_request(url, '/v1/metadata', {
        'type': 'pg_add_source',
        'args': {
            'name': 'default',
            'configuration': {'connection_info': {'database_url': source_url}},
            'replace_configuration': True,
        },
    })
    values = ', '.join(f"('{role}')" for role in WHOAMI_ROLES)
    _admin_request(url, '/v2/query', {
        'type': 'run_sql',
        'args': {
            'source': 'default',
            'sql': f'CREATE TABLE whoami (role TEXT PRIMARY KEY); INSERT INTO whoami (role) VALUES {values};',
        },
    })
    permissions = [
        {
            'type': 'pg_create_select_permission',
            'args': {
                'source': 'default',
                'table': 'whoami',
                'role': role,
                'permission': {'columns': ['role'], 'filter': {'role': {'_eq': 'X-Hasura-Role'}}},
            },
        }
        for role in WHOAMI_ROLES
    ]
    _admin_request(url, '/v1/metadata', {
        'type': 'bulk',
        'args': [
            {'type': 'pg_track_table', 'args': {'source': 'default', 'table': 'whoami'}},
            *permissions,
            {
                'type': 'pg_create_insert_permission',
                'args': {
                    'source': 'default',
                    'table': 'whoami',
                    'role': 'anon',
                    'permission': {'columns': ['role'], 'check': {}, 'backend_only': True},
                },
            },
        ],
    })


def _observe(response: requests.Response) -> str:
    body = response.json()
    if 'errors' in body:
        return 'E:' + body['errors'][0]['extensions']['code']
    roles = [row['role'] for row in body['data']['whoami']]
    if roles == WHOAMI_ROLES:
        return 'admin'
    assert len(roles) == 1, body
    return roles[0]


@pytest.fixture(scope='class')
def auth_scenarios(request, hge_bin, metadata_schema_url, owner_engine, runner_engine, worker_id, tmp_path_factory):
    if not hge_bin:
        pytest.skip('This test requires --hge-bin')

    source = fixtures.postgres.create_schema(request, owner_engine, runner_engine, 'source')

    hook = ThreadingHTTPServer(('127.0.0.1', 0), _HookHandler)
    hook_thread = threading.Thread(target=hook.serve_forever, daemon=True)
    hook_thread.start()
    try:
        context = AuthScenariosContext(
            hge_bin=hge_bin,
            metadata_url=metadata_schema_url,
            worker_id=worker_id,
            hook_url=f'http://127.0.0.1:{hook.server_port}/',
            log_dir=tmp_path_factory.mktemp('auth-scenarios'),
        )
        # Configure the shared metadata through an engine with an admin secret;
        # the engines started by the tests then load it.
        with context.running(Scenario('A3', secret=True)) as url:
            _setup_metadata(url, source.url)
        yield context
    finally:
        hook.shutdown()
        hook.server_close()
        hook_thread.join(timeout=5)


class TestAuthScenarios:

    @pytest.mark.parametrize('scenario,expected', REQUEST_MATRIX, ids=[str(s) for s, _ in REQUEST_MATRIX])
    def test_request_resolution(self, auth_scenarios: AuthScenariosContext, scenario: Scenario, expected: list[str]):
        assert len(expected) == len(REQUESTS)
        with auth_scenarios.running(scenario) as url:
            for (label, make_headers), want in zip(REQUESTS, expected):
                headers = make_headers()
                got = _observe(requests.post(url + '/v1/graphql', json={'query': WHOAMI_QUERY}, headers=headers, timeout=10))
                assert got == want, f'{scenario}, {label}'

                # Admin-only APIs are available exactly when the request resolves to admin.
                metadata = requests.post(url + '/v1/metadata', json={'type': 'export_metadata', 'args': {}}, headers=headers, timeout=10)
                assert (metadata.status_code == 200) == (want == 'admin'), f'{scenario}, {label}: {metadata.text}'

    @pytest.mark.parametrize('scenario,needles', STARTUP_FAILURES, ids=[str(s) for s, _ in STARTUP_FAILURES])
    def test_startup_failure(self, auth_scenarios: AuthScenariosContext, scenario: Scenario, needles: list[str]):
        port = ports.find_free_port(auth_scenarios.worker_id)
        result = subprocess.run(
            auth_scenarios.args(port), env=auth_scenarios.env(scenario),
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            encoding='utf-8', errors='replace', timeout=60,
        )
        assert result.returncode != 0, result.stdout
        for needle in needles:
            assert needle in result.stdout, result.stdout

    def test_unauthorized_role_never_gets_backend_only_access(self, auth_scenarios: AuthScenariosContext):
        """B2 never grants backend-only permissions; A4 grants them only with the admin secret."""
        mutation = {'query': 'mutation { insert_whoami(objects: {role: "backend-only"}) { affected_rows } }'}
        backend_only = {'x-hasura-use-backend-only-permissions': 'true'}

        with auth_scenarios.running(Scenario('B2', secret=True, disabled=True, unauthorized_role='anon')) as url:
            for headers in [backend_only, {**backend_only, 'x-hasura-admin-secret': ADMIN_SECRET, 'x-hasura-role': 'anon'}]:
                response = requests.post(url + '/v1/graphql', json=mutation, headers=headers, timeout=10).json()
                assert response.get('errors', [{}])[0].get('extensions', {}).get('code') == 'validation-failed', response

        with auth_scenarios.running(Scenario('A4', secret=True, unauthorized_role='anon')) as url:
            response = requests.post(url + '/v1/graphql', json=mutation, headers=backend_only, timeout=10).json()
            assert response.get('errors', [{}])[0].get('extensions', {}).get('code') == 'validation-failed', response

            # The permission itself works for the same role once the admin secret is sent.
            headers = {**backend_only, 'x-hasura-admin-secret': ADMIN_SECRET, 'x-hasura-role': 'anon'}
            response = requests.post(url + '/v1/graphql', json=mutation, headers=headers, timeout=10).json()
            assert response == {'data': {'insert_whoami': {'affected_rows': 1}}}, response
            _admin_request(url, '/v2/query', {
                'type': 'run_sql',
                'args': {'source': 'default', 'sql': "DELETE FROM whoami WHERE role = 'backend-only';"},
            })

    def test_websocket_uses_unauthorized_role(self, auth_scenarios: AuthScenariosContext):
        """connection_init headers cannot escalate past the unauthorized role in B2."""
        with auth_scenarios.running(Scenario('B2', secret=True, disabled=True, unauthorized_role='anon')) as url:
            for init_headers in [{}, {'x-hasura-admin-secret': ADMIN_SECRET, 'x-hasura-role': 'admin'}]:
                ws = websocket.create_connection(url.replace('http', 'ws', 1) + '/v1/graphql', timeout=10)
                try:
                    ws.send(json.dumps({'type': 'connection_init', 'payload': {'headers': init_headers}}))
                    ws.send(json.dumps({'type': 'start', 'id': '1', 'payload': {'query': WHOAMI_QUERY}}))
                    for _ in range(20):
                        frame = json.loads(ws.recv())
                        if frame['type'] == 'data':
                            assert frame['payload'] == {'data': {'whoami': [{'role': 'anon'}]}}, (init_headers, frame)
                            break
                        assert frame['type'] in ('connection_ack', 'ka'), (init_headers, frame)
                    else:
                        pytest.fail(f'No data frame received for {init_headers}')
                finally:
                    ws.close()
