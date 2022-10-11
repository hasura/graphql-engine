import collections
import os
import pytest
import re
import sqlalchemy
import sys
import threading
import time
from typing import Optional
import uuid

import auth_webhook_server
from context import HGECtx, HGECtxGQLServer, ActionsWebhookServer, EvtsWebhookServer, GQLWsClient, PytestConf, GraphQLWSClient
import fixtures.hge
import graphql_server
import ports

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
    parser.addoption(
        "--hge-webhook", metavar="HGE_WEBHOOK", help="url for graphql-engine's access control webhook", required=False
    )
    parser.addoption(
        "--test-webhook-insecure", action="store_true",
        help="Run Test cases for insecure https webhook"
    )
    parser.addoption(
        "--test-webhook-request-context", action="store_true",
        help="Run Test cases for testing webhook request context"
    )
    parser.addoption(
        "--hge-jwt-key-file", metavar="HGE_JWT_KEY_FILE", help="File containing the private key used to encode jwt tokens using RS512 algorithm", required=False
    )
    parser.addoption(
        "--hge-jwt-conf", metavar="HGE_JWT_CONF", help="The JWT conf", required=False
    )

    parser.addoption(
        "--test-ws-init-cookie",
        metavar="read|noread",
        required=False,
        help="Run testcases for testing cookie sending over websockets"
    )

    parser.addoption(
        "--test-hge-scale-url",
        metavar="<url>",
        required=False,
        help="Run testcases for horizontal scaling"
    )

    parser.addoption(
        "--test-logging",
        action="store_true",
        default=False,
        required=False,
        help="Run testcases for logging"
    )

    parser.addoption(
        "--test-startup-db-calls",
        action="store_true",
        default=False,
        required=False,
        help="Run testcases for startup database calls"
    )

    parser.addoption(
        "--test-jwk-url",
        action="store_true",
        default=False,
        required=False,
        help="Run testcases for JWK url behaviour"
    )

    parser.addoption(
        "--accept",
        action="store_true",
        default=False,
        required=False,
        help="Accept any failing test cases from YAML files as correct, and write the new files out to disk."
    )
    parser.addoption(
        "--skip-schema-teardown",
        action="store_true",
        default=False,
        required=False,
        help="""
Skip tearing down the schema/Hasura metadata after tests. This option may result in test failures if the schema
has to change between the list of tests to be run
"""
    )
    parser.addoption(
        "--skip-schema-setup",
        action="store_true",
        default=False,
        required=False,
        help="""
Skip setting up schema/Hasura metadata before tests.
This option may result in test failures if the schema has to change between the list of tests to be run
"""
    )

    parser.addoption(
        "--avoid-error-message-checks",
        action="store_true",
        default=False,
        required=False,
        help="""
    This option when set will ignore disparity in error messages between expected and response outputs.
    Used basically in version upgrade/downgrade tests where the error messages may change
    """
    )

    parser.addoption(
        "--collect-upgrade-tests-to-file",
        metavar="<path>",
        required=False,
        help="When used along with collect-only, it will write the list of upgrade tests into the file specified"
    )

    parser.addoption(
        "--test-unauthorized-role",
        action="store_true",
        help="Run testcases for unauthorized role",
    )

    parser.addoption(
        "--test-no-cookie-and-unauth-role",
        action="store_true",
        help="Run testcases for no unauthorized role and no cookie jwt header set (cookie auth is set as part of jwt config upon engine startup)",
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
        "--test-auth-webhook-header",
        action="store_true",
        default=False,
        required=False,
        help="Run testcases for auth webhook header forwarding"
    )

    parser.addoption(
        "--test-read-only-source",
        action="store_true",
        default=False,
        required=False,
        help="Run testcases with a read-only database source"
    )


#By default,
#1) Set test grouping to by class (--dist=loadfile)
#2) Set default parallelism to one
def pytest_cmdline_preparse(config, args):
    worker = os.environ.get('PYTEST_XDIST_WORKER')
    if 'xdist' in sys.modules and not worker:  # pytest-xdist plugin
        num = 1
        args[:] = ['--dist=loadfile', f'-n{num}'] + args

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
        if config.getoption('-n', default=None):
            xdist_threads = config.getoption('-n')
            assert config.getoption('--hge-bin') or xdist_threads <= len(config.hge_url_list), "Not enough hge_urls specified, Required " + str(xdist_threads) + ", got " + str(len(config.hge_url_list))
            assert xdist_threads <= len(config.pg_url_list), "Not enough pg_urls specified, Required " + str(xdist_threads) + ", got " + str(len(config.pg_url_list))

@pytest.hookimpl()
def pytest_report_collectionfinish(config, startdir, items):
    """
    Collect server upgrade tests to the given file
    """
    tests_file = config.getoption('--collect-upgrade-tests-to-file')
    tests = collections.OrderedDict()
    if tests_file:
        def is_upgrade_test(item):
            # Check if allow_server_upgrade_tests marker are present
            # skip_server_upgrade_tests marker is not present
            return item.get_closest_marker('allow_server_upgrade_test') \
                and not item.get_closest_marker('skip_server_upgrade_test')
        with open(tests_file,'w') as f:
            upgrade_items = filter(is_upgrade_test, items)
            for item in upgrade_items:
                # This test should be run separately,
                # since its schema setup has function scope
                if 'per_method_tests_db_state' in item.fixturenames:
                    tests[item.nodeid] = True
                elif any([ (x in item.fixturenames)
                    for x in
                    [ 'per_class_tests_db_state',
                      'per_class_db_schema_for_mutation_tests'
                    ]
                ]):
                    # For this test, schema setup has class scope
                    # We can run a class of these tests at a time
                    tests[item.parent.nodeid] = True
                # Assume tests can only be run separately
                else:
                    tests[item.nodeid] = True
            for test in tests.keys():
                f.write(test + '\n')
    return ''

@pytest.hookimpl(optionalhook=True)
def pytest_configure_node(node):
    if is_help_option_present(node.config):
        return
    if not node.config.getoption('--hge-bin'):
        node.workerinput["hge-url"] = node.config.hge_url_list.pop()
    node.workerinput["pg-url"] = node.config.pg_url_list.pop()

def run_on_current_backend(request: pytest.FixtureRequest):
    current_backend = request.config.getoption('--backend')
    # Currently, we default all tests to run on Postgres with or without a --backend flag.
    # As our test suite develops, we may consider running backend-agnostic tests on all
    # backends, unless a specific `--backend` flag is passed.
    desired_backends = set(name for marker in request.node.iter_markers('backend') for name in marker.args) or set(['postgres'])
    return current_backend in desired_backends

def per_backend_tests_fixture(request: pytest.FixtureRequest):
    """
    This fixture ignores backend-specific tests unless the relevant --backend flag has been passed.
    """
    if not run_on_current_backend(request):
        desired_backends = set(name for marker in request.node.iter_markers('backend') for name in marker.args)
        pytest.skip('Skipping test. This test can run on ' + ', '.join(desired_backends) + '.')

@pytest.fixture(scope='class', autouse=True)
def per_backend_test_class(request: pytest.FixtureRequest):
    return per_backend_tests_fixture(request)

@pytest.fixture(scope='function', autouse=True)
def per_backend_test_function(request: pytest.FixtureRequest):
    return per_backend_tests_fixture(request)

@pytest.fixture(scope='session')
def pg_version(request) -> int:
    pg_url: str = request.config.workerinput["pg-url"]
    with sqlalchemy.create_engine(pg_url).connect() as connection:
        row = connection.execute('show server_version_num').fetchone()
        if not row:
            raise Exception('Could not get the PostgreSQL version.')
        return int(row['server_version_num']) // 10000

@pytest.fixture(scope='class')
def pg_url(request) -> str:
    return request.config.workerinput["pg-url"]

# Any test caught by this would also be caught by `hge_skip_function`, but
# this is faster.
@pytest.fixture(scope='class', autouse=True)
def hge_skip_class(request: pytest.FixtureRequest, hge_server: Optional[str]):
    hge_skip(request, hge_server)

@pytest.fixture(scope='function', autouse=True)
def hge_skip_function(request: pytest.FixtureRequest, hge_server: Optional[str]):
    hge_skip(request, hge_server)

def hge_skip(request: pytest.FixtureRequest, hge_server: Optional[str]):
    # Let `hge_server` manage this stuff.
    if hge_server:
        return
    # Ensure that the correct environment variables have been set for the given test.
    hge_marker_env: dict[str, str] = {marker.args[0]: marker.args[1] for marker in request.node.iter_markers('hge_env')}
    incorrect_env = {name: value for name, value in hge_marker_env.items() if os.getenv(name) != value}
    if len(incorrect_env) > 0:
        pytest.skip(
            'This test expects the following environment variables: '
            + ', '.join([f'{name!r} = {value!r}' for name, value in incorrect_env.items()]))

@pytest.fixture(scope='class')
def postgis(pg_url):
    with sqlalchemy.create_engine(pg_url).connect() as connection:
        connection.execute('CREATE EXTENSION IF NOT EXISTS postgis')
        connection.execute('CREATE EXTENSION IF NOT EXISTS postgis_topology')
        result = connection.execute('SELECT PostGIS_lib_version() as postgis_version').fetchone()
        if not result:
            raise Exception('Could not detect the PostGIS version.')
        postgis_version: str = result['postgis_version']
        if re.match('^3\\.', postgis_version):
            connection.execute('CREATE EXTENSION IF NOT EXISTS postgis_raster')

@pytest.fixture(scope='session')
def hge_bin(request: pytest.FixtureRequest) -> Optional[str]:
    return request.config.getoption('--hge-bin')  # type: ignore

@pytest.fixture(scope='class')
def hge_port() -> int:
    return fixtures.hge.hge_port()

@pytest.fixture(scope='class')
def hge_url(request: pytest.FixtureRequest, hge_bin: Optional[str], hge_port: int) -> str:
    if hge_bin:
        return f'http://localhost:{hge_port}'
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
    else:
        # If the environment variable is set, use it.
        # This will be used in the event that we start the server outside the test harness.
        # We skip the test if it explicitly requires that we have no admin secret.
        anti_marker = request.node.get_closest_marker('no_admin_secret')
        env_key = os.environ.get('HASURA_GRAPHQL_ADMIN_SECRET')
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
    pg_url: str,
) -> Optional[str]:
    if not hge_bin:
      return None
    return fixtures.hge.hge_server(request, hge_bin, hge_port, hge_url, hge_key, hge_fixture_env, pg_url)

@pytest.fixture(scope='class')
def enabled_apis(request: pytest.FixtureRequest, hge_bin: Optional[str]) -> Optional[set[str]]:
    if hge_bin:
        hge_marker_env: dict[str, str] = {marker.args[0]: marker.args[1] for marker in request.node.iter_markers('hge_env') if marker.args[1] is not None}
        enabled_apis_str = hge_marker_env.get('HASURA_GRAPHQL_ENABLED_APIS')
    else:
        enabled_apis_str = os.environ.get('HASURA_GRAPHQL_ENABLED_APIS')
    if not enabled_apis_str:
        return None
    return set(enabled_apis_str.split(','))

@pytest.fixture(scope='class')
def hge_ctx(request, hge_url, pg_url, hge_key, enabled_apis, hge_server):
    hge_ctx = HGECtx(hge_url, pg_url, hge_key, enabled_apis, request.config)

    yield hge_ctx

    hge_ctx.teardown()
    time.sleep(1)  # TODO why do we sleep here?

@pytest.fixture(scope='class')
@pytest.mark.early
def evts_webhook(hge_fixture_env: dict[str, str]):
    webhook_httpd = EvtsWebhookServer(server_address=('localhost', 5592))
    web_server = threading.Thread(target=webhook_httpd.serve_forever)
    web_server.start()
    hge_fixture_env['EVENT_WEBHOOK_HANDLER'] = webhook_httpd.url
    yield webhook_httpd
    webhook_httpd.shutdown()
    webhook_httpd.server_close()
    web_server.join()

@pytest.fixture(scope='class')
@pytest.mark.early
def actions_fixture(pg_version: int, hge_url: str, hge_key: Optional[str], hge_fixture_env: dict[str, str]):
    if pg_version < 10:
        pytest.skip('Actions are not supported on Postgres version < 10')

    # Start actions' webhook server
    webhook_httpd = ActionsWebhookServer(hge_url, hge_key, server_address=('localhost', 5593))
    web_server = threading.Thread(target=webhook_httpd.serve_forever)
    web_server.start()
    hge_fixture_env['ACTION_WEBHOOK_HANDLER'] = webhook_httpd.url
    yield webhook_httpd
    webhook_httpd.shutdown()
    webhook_httpd.server_close()
    web_server.join()

use_action_fixtures = pytest.mark.usefixtures(
    'actions_fixture',
    'per_class_db_schema_for_mutation_tests',
    'per_method_db_data_for_mutation_tests'
)

@pytest.fixture(scope='class')
@pytest.mark.early
def auth_hook(hge_fixture_env: dict[str, str]):
    server = auth_webhook_server.create_server()
    server_thread = threading.Thread(target = server.serve_forever)
    server_thread.start()
    hge_fixture_env['HASURA_GRAPHQL_AUTH_HOOK'] = 'http://localhost:9876/auth'
    ports.wait_for_port(server.server_port)
    yield server
    auth_webhook_server.stop_server(server)

@pytest.fixture(scope='class')
def pro_tests_fixtures(hge_ctx):
    if not hge_ctx.pro_tests:
        pytest.skip('These tests are meant to be run with --pro-tests set')
        return

@pytest.fixture(scope='class')
@pytest.mark.early
def scheduled_triggers_evts_webhook(hge_fixture_env: dict[str, str]):
    webhook_httpd = EvtsWebhookServer(server_address=('localhost', 5594))
    web_server = threading.Thread(target=webhook_httpd.serve_forever)
    web_server.start()
    hge_fixture_env['SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN'] = webhook_httpd.url
    yield webhook_httpd
    webhook_httpd.shutdown()
    webhook_httpd.server_close()
    web_server.join()

@pytest.fixture(scope='class')
@pytest.mark.early
def gql_server(request, hge_fixture_env: dict[str, str]):
    port = 5000
    hge_urls: list[str] = request.config.getoption('--hge-urls')
    graphql_server.set_hge_urls(hge_urls)
    server = HGECtxGQLServer(port=port)
    server.start_server()
    hge_fixture_env['REMOTE_SCHEMAS_WEBHOOK_DOMAIN'] = server.url
    ports.wait_for_port(port)
    yield server
    server.stop_server()

@pytest.fixture(scope='class')
def ws_client(request, hge_ctx):
    """
    This fixture provides an Apollo GraphQL websockets client
    """
    client = GQLWsClient(hge_ctx, '/v1/graphql')
    time.sleep(0.1)
    yield client
    client.teardown()

@pytest.fixture(scope='class')
def ws_client_graphql_ws(request, hge_ctx):
    """
    This fixture provides an GraphQL-WS client
    """
    client = GraphQLWSClient(hge_ctx, '/v1/graphql')
    time.sleep(0.1)
    yield client
    client.teardown()

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
        skip_setup=False, skip_teardown=False
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
    (skip_setup, skip_teardown) = [
        request.config.getoption('--' + x)
        for x in ['skip-schema-setup', 'skip-schema-teardown']
    ]
    yield from db_context_common(
        request, hge_ctx,
        setup_files_attr, setup_default_file,
        teardown_files_attr, teardown_default_file,
        skip_setup, skip_teardown
    )

def db_context_with_schema_common_new(
    request, hge_ctx,
    setup_files_attr, setup_default_file,
    teardown_files_attr, teardown_default_file,
    setup_sql_file, teardown_sql_file,
    pre_setup_file, post_teardown_file,
):
    (skip_setup, skip_teardown) = [
        request.config.getoption('--' + x)
        for x in ['skip-schema-setup', 'skip-schema-teardown']
    ]
    yield from db_context_common_new(
        request, hge_ctx,
        setup_files_attr, setup_default_file, setup_sql_file,
        teardown_files_attr, teardown_default_file, teardown_sql_file,
        pre_setup_file, post_teardown_file,
        skip_setup, skip_teardown
    )

def db_context_common(
        request, hge_ctx,
        setup_files_attr, setup_default_file,
        teardown_files_attr, teardown_default_file,
        skip_setup=True, skip_teardown=True
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
            skip_setup, skip_teardown
        )
    else:
        yield from setup_and_teardown_v2q(
            request, hge_ctx,
            setup, teardown,
            skip_setup, skip_teardown
        )


def db_context_common_new(
        request, hge_ctx,
        setup_files_attr, setup_default_file, setup_default_sql_file,
        teardown_files_attr, teardown_default_file, teardown_default_sql_file,
        pre_setup_file, post_teardown_file,
        skip_setup=True, skip_teardown=True
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
        skip_setup, skip_teardown
    )

def setup_and_teardown_v1q(
    request, hge_ctx,
    setup_files, teardown_files,
    skip_setup=False, skip_teardown=False
):
    def v1q_f(filepath):
        if os.path.isfile(filepath):
            return hge_ctx.v1q_f(filepath)

    if not skip_setup:
        run_on_elem_or_list(v1q_f, setup_files)
    yield
    # Teardown anyway if any of the tests have failed
    if request.session.testsfailed > 0 or not skip_teardown:
        run_on_elem_or_list(v1q_f, teardown_files)

def setup_and_teardown_v2q(
    request, hge_ctx,
    setup_files, teardown_files,
    skip_setup=False, skip_teardown=False
):
    def v2q_f(filepath):
        if os.path.isfile(filepath):
            return hge_ctx.v2q_f(filepath)

    if not skip_setup:
        run_on_elem_or_list(v2q_f, setup_files)
    yield
    # Teardown anyway if any of the tests have failed
    if request.session.testsfailed > 0 or not skip_teardown:
        run_on_elem_or_list(v2q_f, teardown_files)

def setup_and_teardown(
    request, hge_ctx,
    setup_files, teardown_files,
    sql_schema_setup_file, sql_schema_teardown_file,
    pre_setup_file, post_teardown_file,
    skip_setup=False, skip_teardown=False
):
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
    if not skip_setup:
        run_on_elem_or_list(pre_post_metadataq_f, pre_setup_file)
        run_on_elem_or_list(v2q_f, sql_schema_setup_file)
        run_on_elem_or_list(metadataq_f, setup_files)
    yield
    # Teardown anyway if any of the tests have failed
    if request.session.testsfailed > 0 or not skip_teardown:
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
