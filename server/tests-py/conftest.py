import collections
import http.server
import json
import inspect
import os
import pytest
import socket
import sqlalchemy
import subprocess
import sys
import threading
import time
from typing import Any, Optional
import urllib.parse
import uuid

import auth_webhook_server
from context import ActionsWebhookServer, EvtsWebhookServer, GQLWsClient, GraphQLWSClient, HGECtx, HGECtxGQLServer, HGECtxWebhook, PytestConf
import fixtures.hge
import fixtures.jwt
import fixtures.postgres
import fixtures.tls
import jwk_server
import ports
import webhook
import PortToHaskell

def pytest_addoption(parser):
    parser.addoption(
        "--hge-bin",
        metavar="HGE_BIN",
        required=False,
        help="Hasura GraphQL Engine binary executable",
    )
    parser.addoption(
        "--hge-urls",
        metavar="HGE_URLS",
        help="csv list of urls for graphql-engine",
        required=False,
        nargs='+'
    )
    parser.addoption(
        "--pg-urls", metavar="PG_URLS",
        help="csv list of urls for connecting to Postgres directly",
        required=False,
        nargs='+'
    )

    parser.addoption('--tls-ca-cert', help='The CA certificate used for helper services', required=False)
    parser.addoption('--tls-ca-key', help='The CA key used for helper services', required=False)

    parser.addoption(
        "--test-hge-scale-url",
        metavar="<url>",
        required=False,
        help="Run testcases for horizontal scaling"
    )

    parser.addoption(
        "--test-startup-db-calls",
        action="store_true",
        default=False,
        required=False,
        help="Run testcases for startup database calls"
    )

    parser.addoption(
        "--accept",
        action="store_true",
        default=False,
        required=False,
        help="Accept any failing test cases from YAML files as correct, and write the new files out to disk."
    )

    parser.addoption(
        "--redis-url",
        metavar="REDIS_URL",
        help="redis url for cache server",
        default=False
    )

    parser.addoption(
        "--backend",
        help="run integration tests using a particular backend",
        default="postgres"
    )

    parser.addoption(
        "--pro-tests",
        action="store_true",
        default=False,
        help="Flag to specify if the pro tests are to be run"
    )

    parser.addoption(
        "--port-to-haskell",
        action="store_true",
        default=False,
        required=False,
        help="Rather than running tests, generate .hs modules into the api-tests suite"
    )


#By default,
#1) Set test grouping to by class (--dist=loadscope)
#2) Set default parallelism to one
def pytest_cmdline_preparse(config, args):
    worker = os.environ.get('PYTEST_XDIST_WORKER')
    if 'xdist' in sys.modules and not worker:  # pytest-xdist plugin
        num = 1
        args[:] = ['--dist=loadscope', f'-n{num}'] + args

def pytest_configure(config):
    # Pytest has removed the global pytest.config
    # As a solution we are going to store it in PytestConf.config
    PytestConf.config = config
    if is_help_option_present(config):
        return
    if is_master(config):
        assert not config.getoption('--exitfirst'), 'The "--exitfirst"/"-x" option does not work with xdist.\nSee: https://github.com/pytest-dev/pytest-xdist/issues/54'
        if not (config.getoption('--hge-bin') or config.getoption('--hge-urls')):
            print("either --hge-bin or --hge-urls should be specified")
        if config.getoption('--hge-bin') and config.getoption('--hge-urls'):
            print("only one of --hge-bin or --hge-urls should be specified")
        if not config.getoption('--pg-urls'):
            print("pg-urls should be specified")
        config.hge_url_list = config.getoption('--hge-urls')
        config.pg_url_list = config.getoption('--pg-urls')
        if not config.getoption('--hge-bin') and config.getoption('-n', default=None):
            xdist_threads = config.getoption('-n')
            assert config.getoption('--hge-bin') or xdist_threads <= len(config.hge_url_list), "Not enough hge_urls specified, Required " + str(xdist_threads) + ", got " + str(len(config.hge_url_list))
            assert xdist_threads <= len(config.pg_url_list), "Not enough pg_urls specified, Required " + str(xdist_threads) + ", got " + str(len(config.pg_url_list))

@pytest.hookimpl(optionalhook=True)
def pytest_configure_node(node):
    if is_help_option_present(node.config):
        return
    if not node.config.getoption('--hge-bin'):
        node.workerinput["hge-url"] = node.config.hge_url_list.pop()
        node.workerinput["pg-url"] = node.config.pg_url_list.pop()

@pytest.fixture(scope='class', autouse=True)
def current_backend(request: pytest.FixtureRequest) -> str:
    return request.config.getoption('--backend')  # type: ignore

def run_on_current_backend(request: pytest.FixtureRequest, current_backend: str):
    # Currently, we default all tests to run on Postgres with or without a --backend flag.
    # As our test suite develops, we may consider running backend-agnostic tests on all
    # backends, unless a specific `--backend` flag is passed.
    desired_backends = set(name for marker in request.node.iter_markers('backend') for name in marker.args) or set(['postgres'])
    return current_backend in desired_backends

def per_backend_tests_fixture(request: pytest.FixtureRequest, current_backend: str):
    """
    This fixture ignores backend-specific tests unless the relevant --backend flag has been passed.
    """
    if not run_on_current_backend(request, current_backend):
        desired_backends = set(name for marker in request.node.iter_markers('backend') for name in marker.args)
        pytest.skip('Skipping test. This test can run on ' + ', '.join(desired_backends) + '.')

@pytest.fixture(scope='class', autouse=True)
def per_backend_test_class(request: pytest.FixtureRequest, current_backend: str):
    return per_backend_tests_fixture(request, current_backend)

@pytest.fixture(scope='function', autouse=True)
def per_backend_test_function(request: pytest.FixtureRequest, current_backend: str):
    return per_backend_tests_fixture(request, current_backend)

@pytest.fixture(scope='session')
def owner_engine(request: pytest.FixtureRequest, hge_bin: str):
    # TODO: remove once parallelization work is completed
    #       `hge_bin` will no longer be optional
    if not hge_bin:
        return sqlalchemy.engine.create_engine(request.config.workerinput["pg-url"])  # type: ignore

    return fixtures.postgres.owner_engine(request)

@pytest.fixture(scope='session')
def runner_engine(owner_engine: sqlalchemy.engine.Engine):
    return fixtures.postgres.runner_engine(owner_engine)

@pytest.fixture(scope='class')
def metadata_schema_url(
    request: pytest.FixtureRequest,
    owner_engine: sqlalchemy.engine.Engine,
    runner_engine: sqlalchemy.engine.Engine,
    hge_bin: Optional[str],
):
    # TODO: remove once parallelization work is completed
    #       `hge_bin` will no longer be optional
    if not hge_bin:
        return request.config.workerinput["pg-url"]  # type: ignore

    return fixtures.postgres.metadata_schema_url(request, owner_engine, runner_engine)

@pytest.fixture(scope='class')
def source_backend(
    request: pytest.FixtureRequest,
    owner_engine: sqlalchemy.engine.Engine,
    runner_engine: sqlalchemy.engine.Engine,
    hge_ctx_fixture: HGECtx,
    hge_bin: Optional[str],
):
    # TODO: remove once parallelization work is completed
    #       `hge_bin` will no longer be optional
    if not hge_bin:
        yield None
        return

    yield from fixtures.postgres.source_backend(request, owner_engine, runner_engine, hge_ctx_fixture)

@pytest.fixture(scope='class')
def add_source(
    request: pytest.FixtureRequest,
    owner_engine: sqlalchemy.engine.Engine,
    runner_engine: sqlalchemy.engine.Engine,
    hge_ctx_fixture: HGECtx,
    hge_bin: Optional[str],
):
    # TODO: remove once parallelization work is completed
    #       `hge_bin` will no longer be optional
    if not hge_bin:
        def ignoring_errors(f):
            def ignoring_errors_impl(*args, **kwargs):
                try:
                    f(*args, **kwargs)
                except:
                    pass
            return ignoring_errors_impl

        def impl(name: str, read_only: bool = False, customization: Any = None) -> fixtures.postgres.Backend:
            if read_only:
                raise Exception('Cannot add a read-only source.')

            if name == 'pg1':
                env_var = 'HASURA_GRAPHQL_PG_SOURCE_URL_1'
            elif name == 'pg2' or name == 'postgres':
                env_var = 'HASURA_GRAPHQL_PG_SOURCE_URL_2'
            else:
                raise Exception(f'Cannot add source {name}.')

            hge_ctx_fixture.v1metadataq({
                'type': 'pg_add_source',
                'args': {
                    'name': name,
                    'configuration': {
                        'connection_info': {
                            'database_url': {
                                'from_env': env_var,
                            },
                        },
                    },
                    'customization': customization,
                },
            })
            request.addfinalizer(ignoring_errors(lambda: hge_ctx_fixture.v1metadataq({
                'type': 'pg_drop_source',
                'args': {
                    'name': name,
                },
            })))

            engine = sqlalchemy.create_engine(os.environ[env_var])
            return fixtures.postgres.Backend(name, engine)

        return impl

    def impl(name: str, read_only: bool = False, customization: Any = None) -> fixtures.postgres.Backend:
        backend = fixtures.postgres.create_schema(request, owner_engine, runner_engine, f'source_{name}', read_only)
        fixtures.postgres.add_source(hge_ctx_fixture, backend, name, customization)
        request.addfinalizer(lambda: fixtures.postgres.drop_source(hge_ctx_fixture, name))
        return backend

    return impl

@pytest.fixture(scope='class')
def postgis(owner_engine: sqlalchemy.engine.Engine, source_backend: fixtures.postgres.Backend):
    return fixtures.postgres.postgis(owner_engine, source_backend)

@pytest.fixture(scope='session')
def tls_ca_configuration(request: pytest.FixtureRequest, tmp_path_factory: pytest.TempPathFactory) -> Optional[fixtures.tls.TLSCAConfiguration]:
    cert_file: Optional[str] = request.config.getoption('--tls-ca-cert')  # type: ignore
    key_file: Optional[str] = request.config.getoption('--tls-ca-key')  # type: ignore
    if cert_file and key_file:
        tmp_path = tmp_path_factory.mktemp('tls')
        return fixtures.tls.TLSCAConfiguration(cert_file, key_file, tmp_path)
    else:
        return None

# TODO: remove once parallelization work is completed
@pytest.fixture(scope='class', autouse=True)
def hge_skip(request: pytest.FixtureRequest, hge_server: Optional[Any], hge_fixture_env: dict[str, str]):
    # Let `hge_server` manage this stuff.
    if hge_server:
        return

    # Ensure that the correct environment variables have been set for the given test.
    hge_marker_env: dict[str, str] = {marker.args[0]: marker.args[1] for marker in request.node.iter_markers('hge_env')}
    hge_env = {**hge_marker_env, **hge_fixture_env}
    incorrect_env = {name: value for name, value in hge_env.items() if os.getenv(name) != value}
    if len(incorrect_env) > 0:
        pytest.skip(
            'This test expects the following environment variables: '
            + ', '.join([f'{name!r} = {value!r} (not {os.getenv(name)!r})' for name, value in incorrect_env.items()]))

    # Make sure that if there's a webhook, the test expects that there's a webhook
    if 'HASURA_GRAPHQL_AUTH_HOOK' in os.environ:
        if 'HASURA_GRAPHQL_AUTH_HOOK' not in hge_fixture_env:
            pytest.skip('HGE expects a running webhook, but this test does not provide one.')
        if os.environ['HASURA_GRAPHQL_AUTH_HOOK'] != hge_fixture_env['HASURA_GRAPHQL_AUTH_HOOK']:
            pytest.skip(f'HGE expects a running webhook at {os.environ["HASURA_GRAPHQL_AUTH_HOOK"]}, but this test provides one at {hge_fixture_env["HASURA_GRAPHQL_AUTH_HOOK"]}.')

@pytest.fixture(scope='session')
def hge_bin(request: pytest.FixtureRequest) -> Optional[str]:
    return request.config.getoption('--hge-bin')  # type: ignore

@pytest.fixture(scope='class')
def hge_port(worker_id: str) -> int:
    return fixtures.hge.hge_port(worker_id)

@pytest.fixture(scope='class')
def hge_url(request: pytest.FixtureRequest, hge_bin: Optional[str], hge_port: int) -> str:
    if hge_bin:
        return f'http://localhost:{hge_port}'
    # TODO: remove once parallelization work is completed
    #       `hge_bin` will no longer be optional
    else:
        return request.config.workerinput['hge-url']  # type: ignore

# A hack to inject environment variables from other fixtures into HGE.
# All of this is because we cannot cleanly express dependencies between
# fixtures of the form "this loads before that, IF that is loaded".
#
# This is marked as "early" so the `fixture-order` plugin ensures that it is
# loaded _before_ `hge_server`. Similarly, any fixture using it must be at
# the same scope level and also marked as "early", to ensure that it is
# mutated before `hge_server` uses the data.
#
# In short, we use `early` to ensure that writes happen before reads.
@pytest.fixture(scope='class')
@pytest.mark.early
def hge_fixture_env() -> dict[str, str]:
    return {}

@pytest.fixture(scope='class')
def hge_key(
    request: pytest.FixtureRequest,
    hge_bin: Optional[str],
) -> Optional[str]:
    marker = request.node.get_closest_marker('admin_secret')
    if hge_bin:
        # If the test requests an admin secret, generate one.
        return str(uuid.uuid4()) if marker else None
    # TODO: remove once parallelization work is completed
    #       `hge_bin` will no longer be optional
    else:
        # If the environment variable is set, use it.
        # This will be used in the event that we start the server outside the test harness.
        # We skip the test if it explicitly requires that we have no admin secret.
        super_marker = request.node.get_closest_marker('requires_an_admin_secret')
        anti_marker = request.node.get_closest_marker('no_admin_secret')
        env_key = os.environ.get('HASURA_GRAPHQL_ADMIN_SECRET')
        if super_marker and not env_key:
            pytest.skip('This test requires that the admin secret is set.')
        if anti_marker and env_key:
            pytest.skip('This test requires that the admin secret is not set.')
        return env_key

@pytest.fixture(scope='class')
def hge_server(
    request: pytest.FixtureRequest,
    hge_bin: Optional[str],
    hge_port: int,
    hge_url: str,
    hge_key: Optional[str],
    hge_fixture_env: dict[str, str],
    metadata_schema_url: str,
) -> Optional[subprocess.Popen[bytes]]:
    # TODO: remove once parallelization work is completed
    #       `hge_bin` will no longer be optional
    if not hge_bin:
      return None
    return fixtures.hge.hge_server(request, hge_bin, hge_port, hge_url, hge_key, hge_fixture_env, metadata_schema_url)

@pytest.fixture(scope='class')
def enabled_apis(request: pytest.FixtureRequest, hge_bin: Optional[str]) -> Optional[set[str]]:
    if hge_bin:
        hge_marker_env: dict[str, str] = {marker.args[0]: marker.args[1] for marker in request.node.iter_markers('hge_env') if marker.args[1] is not None}
        enabled_apis_str = hge_marker_env.get('HASURA_GRAPHQL_ENABLED_APIS')
    # TODO: remove once parallelization work is completed
    #       `hge_bin` will no longer be optional
    else:
        enabled_apis_str = os.environ.get('HASURA_GRAPHQL_ENABLED_APIS')
    if not enabled_apis_str:
        return None
    return set(enabled_apis_str.split(','))

@pytest.fixture(scope='class')
def hge_ctx_fixture(
    request: pytest.FixtureRequest,
    hge_url: str,
    hge_bin: Optional[str],
    metadata_schema_url: str,
    hge_key: Optional[str],
    enabled_apis: Optional[set[str]],
):
    # This madness allows us to figure out whether there is a webhook running.
    # We need this information because we dynamically decide how we run queries according to the authentication method.
    # This is probably terrible, but refactoring that logic would require rewriting every test.
    webhook: Optional[HGECtxWebhook] = None
    if webhook_server.__name__ in request.fixturenames:
        webhook = request.getfixturevalue(webhook_server.__name__)
    elif 'query_echo_webhook' in request.fixturenames: # in test_webhook_request_context.py
        webhook = HGECtxWebhook(tls_trust=None)

    hge_ctx = HGECtx(
        hge_url=hge_url,
        metadata_schema_url=metadata_schema_url,
        hge_key=hge_key,
        webhook=webhook,
        enabled_apis=enabled_apis,
        # TODO: remove once parallelization work is completed
        #       `hge_bin` will no longer be optional
        clear_dbs=not hge_bin,
        config=request.config,
    )

    yield hge_ctx

    hge_ctx.teardown()
    time.sleep(1)  # TODO why do we sleep here?

# tie everything together
@pytest.fixture(scope='class')
def hge_ctx(
    hge_ctx_fixture: HGECtx,
    hge_server,
    hge_url,
    source_backend,
):
    return hge_ctx_fixture

@pytest.fixture(scope='class')
@pytest.mark.early
def evts_webhook(hge_fixture_env: dict[str, str]):
    server = EvtsWebhookServer(extract_server_address_from('EVENT_WEBHOOK_HANDLER'))
    thread = threading.Thread(target=server.serve_forever)
    thread.start()
    print(f'{evts_webhook.__name__} server started on {server.url}')
    hge_fixture_env['EVENT_WEBHOOK_HANDLER'] = server.url
    yield server
    server.shutdown()
    server.server_close()
    thread.join()

@pytest.fixture(scope='class')
@pytest.mark.early
def actions_fixture(hge_url: str, hge_key: Optional[str], hge_fixture_env: dict[str, str]):
    # Start actions' webhook server
    server = ActionsWebhookServer(hge_url, hge_key, server_address=extract_server_address_from('ACTION_WEBHOOK_HANDLER'))
    thread = threading.Thread(target=server.serve_forever)
    thread.start()
    print(f'{actions_fixture.__name__} server started on {server.url}')
    hge_fixture_env['ACTION_WEBHOOK_HANDLER'] = server.url
    yield server
    server.shutdown()
    server.server_close()
    thread.join()

use_action_fixtures = pytest.mark.usefixtures(
    'actions_fixture',
    'per_class_db_schema_for_mutation_tests',
    'per_method_db_data_for_mutation_tests'
)

@pytest.fixture(scope='class')
@pytest.mark.early
def auth_hook(hge_fixture_env: dict[str, str]):
    server = auth_webhook_server.create_server(extract_server_address_from('HASURA_GRAPHQL_AUTH_HOOK'))
    thread = threading.Thread(target = server.serve_forever)
    thread.start()
    print(f'{auth_hook.__name__} server started on {server.url}')
    hge_fixture_env['HASURA_GRAPHQL_AUTH_HOOK'] = server.url + '/auth'
    ports.wait_for_port(server.server_port)
    yield server
    auth_webhook_server.stop_server(server)

@pytest.fixture(scope='class')
@pytest.mark.early
def webhook_server(
    request: pytest.FixtureRequest,
    hge_bin: Optional[str],
    hge_fixture_env: dict[str,str],
    tls_ca_configuration: Optional[fixtures.tls.TLSCAConfiguration],
):
    server_address = extract_server_address_from('HASURA_GRAPHQL_AUTH_HOOK')

    scheme = None
    # TODO: remove once parallelization work is completed
    #       `hge_bin` will no longer be optional
    if not hge_bin:
        scheme = str(urllib.parse.urlparse(os.getenv('HASURA_GRAPHQL_AUTH_HOOK')).scheme)
    if tls_ca_configuration:
        if scheme is not None and scheme != 'https':
            pytest.skip(f'Cannot run the remote schema server with TLS; HGE is configured to talk to it over "{scheme}".')

        server = http.server.HTTPServer(server_address, webhook.Handler)
        use_tls = request.node.get_closest_marker('no_tls_webhook_server') is None
        if use_tls:
            insecure = request.node.get_closest_marker('tls_insecure_certificate') is not None
            tls_trust = fixtures.tls.TLSTrust.INSECURE if insecure else fixtures.tls.TLSTrust.SECURE
            tls_ca_configuration.configure(server, tls_trust)
        else:
            tls_trust = None
    else:
        if scheme is not None and scheme != 'http':
            pytest.skip(f'Cannot run the remote schema server without TLS; HGE is configured to talk to it over "{scheme}".')
        if request.node.get_closest_marker('tls_webhook_server') is not None:
            pytest.skip('Only running this test with TLS enabled; skipping the version with TLS disabled.')
        server = http.server.HTTPServer(server_address, webhook.Handler)
        tls_trust = None

    thread = threading.Thread(target=server.serve_forever)
    thread.start()
    request.addfinalizer(server.shutdown)

    scheme = 'https' if tls_trust else 'http'
    # We must use 'localhost' and not `server.server_address[0]`
    # because when using TLS, we need a domain name, not an IP address.
    host = 'localhost'
    port = server.server_address[1]
    hge_fixture_env['HASURA_GRAPHQL_AUTH_HOOK'] = f'{scheme}://{host}:{port}/'
    ports.wait_for_port(port)

    return HGECtxWebhook(tls_trust=tls_trust)

@pytest.fixture(scope='class')
def pro_tests_fixtures(hge_ctx):
    if not hge_ctx.pro_tests:
        pytest.skip('These tests are meant to be run with --pro-tests set')

@pytest.fixture(scope='class')
@pytest.mark.early
def scheduled_triggers_evts_webhook(hge_fixture_env: dict[str, str]):
    server = EvtsWebhookServer(extract_server_address_from('SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN'))
    thread = threading.Thread(target=server.serve_forever)
    thread.start()
    print(f'{scheduled_triggers_evts_webhook.__name__} server started on {server.url}')
    hge_fixture_env['SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN'] = server.url
    yield server
    server.shutdown()
    server.server_close()
    thread.join()

# This will use TLS on CI, and won't when run locally. Unfortunately,
# parameterization doesn't really work (see test_webhook.py and test_tests.py
# for details), so we shall avoid it in favor of just testing what we can.
@pytest.fixture(scope='class')
@pytest.mark.early
def gql_server(
    request: pytest.FixtureRequest,
    hge_bin: Optional[str],
    hge_url: str,
    hge_fixture_env: dict[str,str],
    tls_ca_configuration: Optional[fixtures.tls.TLSCAConfiguration],
):
    server_address = extract_server_address_from('REMOTE_SCHEMAS_WEBHOOK_DOMAIN')
    scheme = None
    # TODO: remove once parallelization work is completed
    #       `hge_bin` will no longer be optional
    if not hge_bin:
        scheme = str(urllib.parse.urlparse(os.getenv('REMOTE_SCHEMAS_WEBHOOK_DOMAIN')).scheme)

        if socket.socket(socket.AF_INET, socket.SOCK_STREAM).connect_ex(server_address) == 0:
            # The server is already running. This might be the case if we're running the server upgrade/downgrade tests.
            # In this case, skip starting it and rely on the external service.
            url = f'http://{server_address[0]}:{server_address[1]}'
            hge_fixture_env['REMOTE_SCHEMAS_WEBHOOK_DOMAIN'] = url
            # Create an object with a field named "url", set to `url`.
            return collections.namedtuple('ExternalGraphQLServer', ['url'])(url)

    if tls_ca_configuration:
        if scheme is not None and scheme != 'https':
            pytest.skip(f'Cannot run the remote schema server with TLS; HGE is configured to talk to it over "{scheme}".')
    else:
        if scheme is not None and scheme != 'http':
            pytest.skip(f'Cannot run the remote schema server without TLS; HGE is configured to talk to it over "{scheme}".')

    hge_urls: list[str] = request.config.getoption('--hge-urls') or [hge_url]  # type: ignore

    server = HGECtxGQLServer(
        server_address=server_address,
        tls_ca_configuration=tls_ca_configuration,
        hge_urls=hge_urls,
    )
    server.start_server()
    print(f'{gql_server.__name__} server started on {server.url}')
    hge_fixture_env['REMOTE_SCHEMAS_WEBHOOK_DOMAIN'] = server.url
    request.addfinalizer(server.stop_server)
    return server

@pytest.fixture(scope='class')
def ws_client(hge_ctx):
    """
    This fixture provides an Apollo GraphQL websockets client
    """
    client = GQLWsClient(hge_ctx, '/v1/graphql')
    time.sleep(0.1)
    yield client
    client.teardown()

@pytest.fixture(scope='class')
def ws_client_graphql_ws(hge_ctx):
    """
    This fixture provides an GraphQL-WS client
    """
    client = GraphQLWSClient(hge_ctx, '/v1/graphql')
    time.sleep(0.1)
    yield client
    client.teardown()

@pytest.fixture(scope='class')
@pytest.mark.early
def jwt_configuration(
    request: pytest.FixtureRequest,
    tmp_path_factory: pytest.TempPathFactory,
    hge_fixture_env: dict[str, str],
) -> Optional[fixtures.jwt.JWTConfiguration]:
    marker = request.node.get_closest_marker('jwt')
    if not marker:
        raise Exception('JWT configuration is required.')

    algorithm = marker.args[0]
    try:
        configuration = marker.args[1]
    except IndexError:
        configuration = {}

    tmp_path = tmp_path_factory.mktemp('jwt')
    match algorithm:
        case 'rsa':
            configuration = fixtures.jwt.init_rsa(tmp_path, configuration)
        case 'ed25519':
            configuration = fixtures.jwt.init_ed25519(tmp_path, configuration)
        case 'es':
            configuration = fixtures.jwt.init_es256(tmp_path, configuration)
        case _:
            raise Exception(f'Unsupported JWT configuration: {marker.args!r}')

    hge_fixture_env['HASURA_GRAPHQL_JWT_SECRET'] = json.dumps(configuration.server_configuration)
    return configuration

@pytest.fixture(scope='class')
@pytest.mark.early
def jwk_server_url(request: pytest.FixtureRequest, hge_fixture_env: dict[str, str]):
    path_marker = request.node.get_closest_marker('jwk_path')
    assert path_marker is not None, 'The test must set the `jwk_path` marker.'
    path: str = path_marker.args[0]

    # If the JWK server was started outside, just set the environment variable
    # so that the test is skipped if the value is wrong.
    env_var = os.getenv('JWK_SERVER_URL')
    if env_var:
        hge_fixture_env['HASURA_GRAPHQL_JWT_SECRET'] = '{"jwk_url": "' + env_var + path + '"}'
        return env_var

    server_address = extract_server_address_from('JWK_SERVER_URL')
    server = jwk_server.create_server(server_address)
    thread = threading.Thread(target=server.serve_forever)
    thread.start()
    request.addfinalizer(server.shutdown)

    host = server.server_address[0]
    port = server.server_address[1]
    ports.wait_for_port(port)
    url = f'http://{host}:{port}'
    print(f'{jwk_server_url.__name__} server started on {url}')
    hge_fixture_env['HASURA_GRAPHQL_JWT_SECRET'] = '{"jwk_url": "' + url + path + '"}'
    return url

def extract_server_address_from(env_var: str) -> tuple[str, int]:
    """
    Extracts a server address (a pair of host and port) from the given environment variable.
    If the environment variable doesn't exist, returns `('localhost', 0)`.
    """
    value = os.getenv(env_var)
    if not value:
        return ('localhost', 0)

    url = urllib.parse.urlparse(value)
    if not url.hostname:
        raise Exception(f'Invalid host in {env_var}.')
    if not url.port:
        raise Exception(f'Invalid port in {env_var}.')
    print(f'Found {env_var} = {value!r}, so {inspect.stack()[1].function} server will start on {url.geturl()}')
    return (url.hostname, url.port)

@pytest.fixture(scope='class')
def per_class_tests_db_state(request, hge_ctx):
    """
    Set up the database state for select queries.
    Has a class level scope, since select queries does not change database state
    Expects either `dir()` method which provides the directory
    with `setup.yaml` and `teardown.yaml` files
    Or class variables `setup_files` and `teardown_files` that provides
    the list of setup and teardown files respectively.
    By default, for a postgres backend the setup and teardown is done via
    the `/v1/query` endpoint, to setup using the `/v1/metadata` (metadata setup)
    and `/v2/query` (DB setup), set the `setup_metadata_api_version` to "v2"
    """
    hge_ctx.request = request

    if PytestConf.config.getoption("--port-to-haskell"):
      request.addfinalizer(PortToHaskell.write_tests_to_port)

    yield from db_state_context(request, hge_ctx)

@pytest.fixture(scope='function')
def per_method_tests_db_state(request, hge_ctx):
    """
    This fixture sets up the database state for metadata operations
    Has a function level scope, since metadata operations may change both the schema and data
    Class method/variable requirements are similar to that of per_class_tests_db_state fixture
    """
    yield from db_state_context(request, hge_ctx)

@pytest.fixture(scope='class')
def per_class_db_schema_for_mutation_tests(request, hge_ctx):
    """
    This fixture sets up the database schema for mutations.
    It has a class level scope, since mutations does not change schema.
    Expects either `dir()` class method which provides the directory with `schema_setup.yaml` and `schema_teardown.yaml` files,
    or variables `schema_setup_files` and `schema_teardown_files`
    that provides the list of setup and teardown files respectively
    """

    # setting the default metadata API version to v1
    setup_metadata_api_version = getattr(request.cls, 'setup_metadata_api_version',"v1")

    (setup, teardown, schema_setup, schema_teardown, pre_setup, post_teardown) = [
        hge_ctx.backend_suffix(filename) + ".yaml"
        for filename in ['setup', 'teardown', 'schema_setup', 'schema_teardown', 'pre_setup', 'post_teardown']
    ]

    if hge_ctx.is_default_backend:
        if setup_metadata_api_version == "v1":
            db_context = db_context_with_schema_common(
                request, hge_ctx,
                'schema_setup_files', 'schema_setup.yaml',
                'schema_teardown_files', 'schema_teardown.yaml',
            )
        else:
            db_context = db_context_with_schema_common_new(
                request, hge_ctx,
                'schema_setup_files', setup,
                'schema_teardown_files', teardown,
                schema_setup, schema_teardown,
                pre_setup, post_teardown,
            )
    else:
        db_context = db_context_with_schema_common_new(
            request, hge_ctx,
            'schema_setup_files', setup,
            'schema_teardown_files', teardown,
            schema_setup, schema_teardown,
            pre_setup, post_teardown,
        )
    yield from db_context

@pytest.fixture(scope='function')
def per_method_db_data_for_mutation_tests(request, hge_ctx, per_class_db_schema_for_mutation_tests):
    """
    This fixture sets up the data for mutations.
    Has a function level scope, since mutations may change data.
    Having just the setup file(s), or the teardown file(s) is allowed.
    Expects either `dir()` class method which provides the directory with `values_setup.yaml` and / or `values_teardown.yaml` files.
    The class may provide `values_setup_files` variables which contains the list of data setup files,
    Or the `values_teardown_files` variable which provides the list of data teardown files.
    """

    # Non-default (Postgres) backend tests expect separate setup and schema_setup
    # files for v1/metadata and v2/query requests, respectively.
    (values_setup, values_teardown) = [
        hge_ctx.backend_suffix(filename) + ".yaml"
        for filename in ['values_setup', 'values_teardown']
    ]

    yield from db_context_common(
        request, hge_ctx,
        'values_setup_files', values_setup,
        'values_teardown_files', values_teardown,
    )

def db_state_context(request, hge_ctx):
    # Non-default (Postgres) backend tests expect separate setup and schema_setup
    # files for v1/metadata and v2/query requests, respectively.
    (setup, teardown, schema_setup, schema_teardown, pre_setup, post_teardown) = [
        hge_ctx.backend_suffix(filename) + ".yaml"
        for filename in ['setup', 'teardown', 'schema_setup', 'schema_teardown', 'pre_setup', 'post_teardown']
    ]

    # setting the default metadata API version to v1
    setup_metadata_api_version = getattr(request.cls, 'setup_metadata_api_version',"v1")

    if hge_ctx.is_default_backend:
        if setup_metadata_api_version == "v1":
            # setup the metadata and DB schema using the `/v1/query` endpoint
            db_context = db_context_with_schema_common(
                request, hge_ctx,
                'setup_files', 'setup.yaml',
                'teardown_files', 'teardown.yaml',
            )
        elif setup_metadata_api_version == "v2":
            # setup the metadata using the "/v1/metadata" and the DB schema using the `/v2/query` endpoints
            db_context = db_context_with_schema_common_new(
                request, hge_ctx,
                'setup_files', setup,
                'teardown_files', teardown,
                schema_setup, schema_teardown,
                pre_setup, post_teardown,
            )
        else:
            raise NotImplementedError('Invalid API version.')
    else:
        # setup the metadata using the "/v1/metadata" and the DB schema using the `/v2/query` endpoints
        db_context = db_context_with_schema_common_new(
            request, hge_ctx,
            'setup_files', setup,
            'teardown_files', teardown,
            schema_setup, schema_teardown,
            pre_setup, post_teardown,
        )
    yield from db_context

def db_context_with_schema_common(
    request, hge_ctx,
    setup_files_attr, setup_default_file,
    teardown_files_attr, teardown_default_file,
):
    yield from db_context_common(
        request, hge_ctx,
        setup_files_attr, setup_default_file,
        teardown_files_attr, teardown_default_file,
    )

def db_context_with_schema_common_new(
    request, hge_ctx,
    setup_files_attr, setup_default_file,
    teardown_files_attr, teardown_default_file,
    setup_sql_file, teardown_sql_file,
    pre_setup_file, post_teardown_file,
):
    yield from db_context_common_new(
        request, hge_ctx,
        setup_files_attr, setup_default_file, setup_sql_file,
        teardown_files_attr, teardown_default_file, teardown_sql_file,
        pre_setup_file, post_teardown_file,
    )

def db_context_common(
        request, hge_ctx,
        setup_files_attr, setup_default_file,
        teardown_files_attr, teardown_default_file,
):
    def get_files(attr, default_file):
        files = getattr(request.cls, attr, None)
        if not files:
            files = os.path.join(request.cls.dir(), default_file)
        return files
    setup = get_files(setup_files_attr, setup_default_file)
    teardown = get_files(teardown_files_attr, teardown_default_file)
    if hge_ctx.is_default_backend:
        yield from setup_and_teardown_v1q(
            request, hge_ctx,
            setup, teardown,
        )
    else:
        yield from setup_and_teardown_v2q(
            request, hge_ctx,
            setup, teardown,
        )


def db_context_common_new(
        request, hge_ctx,
        setup_files_attr, setup_default_file, setup_default_sql_file,
        teardown_files_attr, teardown_default_file, teardown_default_sql_file,
        pre_setup_file, post_teardown_file,
):
    def get_files(attr, default_file):
        files = getattr(request.cls, attr, None)
        if not files:
            files = os.path.join(request.cls.dir(), default_file)
        return files
    setup = get_files(setup_files_attr, setup_default_file)
    teardown = get_files(teardown_files_attr, teardown_default_file)
    setup_default_sql_file = os.path.join(request.cls.dir(), setup_default_sql_file)
    teardown_default_sql_file = os.path.join(request.cls.dir(), teardown_default_sql_file)
    pre_setup_default_file = os.path.join(request.cls.dir(), pre_setup_file)
    post_teardown_default_file = os.path.join(request.cls.dir(), post_teardown_file)
    yield from setup_and_teardown(
        request, hge_ctx,
        setup, teardown,
        setup_default_sql_file, teardown_default_sql_file,
        pre_setup_default_file, post_teardown_default_file,
    )

def setup_and_teardown_v1q(
    request, hge_ctx,
    setup_files, teardown_files,
):
    if PytestConf.config.getoption("--port-to-haskell"):
      backend = hge_ctx.backend.title()
      hs_test = PortToHaskell.with_test(request.cls.__qualname__)

      def appendSetupIfExists(name, url):
          def curried(f):
              if os.path.isfile(f):
                  with open(f, 'r') as content:
                      hs_test.add_setup(backend, PortToHaskell.Setup(name, f, url, content.read()))
          return curried

      run_on_elem_or_list(appendSetupIfExists("setup", "/v1/query"), setup_files)

    def v1q_f(filepath):
        if os.path.isfile(filepath):
            return hge_ctx.v1q_f(filepath)

    run_on_elem_or_list(v1q_f, setup_files)
    yield
    run_on_elem_or_list(v1q_f, teardown_files)

def setup_and_teardown_v2q(
    request, hge_ctx,
    setup_files, teardown_files,
):
    if PytestConf.config.getoption("--port-to-haskell"):
      backend = hge_ctx.backend.title()
      hs_test = PortToHaskell.with_test(request.cls.__qualname__)

      def appendSetupIfExists(name, url):
          def curried(f):
              if os.path.isfile(f):
                  with open(f, 'r') as content:
                      hs_test.add_setup(backend, PortToHaskell.Setup(name, f, url, content.read()))
          return curried

      run_on_elem_or_list(appendSetupIfExists("setup", "/v2/query"), setup_files)

    def v2q_f(filepath):
        if os.path.isfile(filepath):
            return hge_ctx.v2q_f(filepath)

    run_on_elem_or_list(v2q_f, setup_files)
    yield
    run_on_elem_or_list(v2q_f, teardown_files)

def setup_and_teardown(
    request, hge_ctx,
    setup_files, teardown_files,
    sql_schema_setup_file, sql_schema_teardown_file,
    pre_setup_file, post_teardown_file,
):
    if PytestConf.config.getoption("--port-to-haskell"):
      backend = hge_ctx.backend.title()
      hs_test = PortToHaskell.with_test(request.cls.__qualname__)

      def appendSetupIfExists(name, url):
          def curried(f):
              if os.path.isfile(f):
                  with open(f, 'r') as content:
                      hs_test.add_setup(backend, PortToHaskell.Setup(name, f, url, content.read()))
          return curried

      run_on_elem_or_list(appendSetupIfExists("pre_setup", "/v1/metadata"), pre_setup_file)
      run_on_elem_or_list(appendSetupIfExists("schema_setup", "/v2/query"), sql_schema_setup_file)
      run_on_elem_or_list(appendSetupIfExists("setup_metadata", "/v1/metadata"), setup_files)

      yield

    else:
      def v2q_f(f):
          if os.path.isfile(f):
              try:
                  hge_ctx.v2q_f(f)
              except AssertionError:
                  try:
                      run_on_elem_or_list(pre_post_metadataq_f, post_teardown_file)
                  except:
                      pass
                  raise
      def metadataq_f(f):
          if os.path.isfile(f):
              try:
                  hge_ctx.v1metadataq_f(f)
              except AssertionError:
                  try:
                      # drop the sql setup, if the metadata calls fail
                      run_on_elem_or_list(v2q_f, sql_schema_teardown_file)
                      run_on_elem_or_list(pre_post_metadataq_f, post_teardown_file)
                  except:
                      pass
                  raise
      def pre_post_metadataq_f(f):
          if os.path.isfile(f):
              hge_ctx.v1metadataq_f(f)
      run_on_elem_or_list(pre_post_metadataq_f, pre_setup_file)
      run_on_elem_or_list(v2q_f, sql_schema_setup_file)
      run_on_elem_or_list(metadataq_f, setup_files)
      yield
      run_on_elem_or_list(metadataq_f, teardown_files)
      run_on_elem_or_list(v2q_f, sql_schema_teardown_file)
      run_on_elem_or_list(pre_post_metadataq_f, post_teardown_file)

def run_on_elem_or_list(f, x):
    if isinstance(x, str):
        return [f(x)]
    elif isinstance(x, list):
        return [f(e) for e in x]

def is_help_option_present(config):
    return any([
        config.getoption(x)
        for x in ['--fixtures','--help', '--collect-only']
    ])

def is_master(config):
    """True if the code running the given pytest.config object is running in a xdist master
    node or not running xdist at all.
    """
    return not hasattr(config, 'workerinput')
