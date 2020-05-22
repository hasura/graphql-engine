import pytest
import time
from context import HGECtx, HGECtxError, ActionsWebhookServer, EvtsWebhookServer, HGECtxGQLServer, GQLWsClient, PytestConf
import threading
import random
from datetime import datetime
import sys
import os
from collections import OrderedDict

def pytest_addoption(parser):
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
        "--hge-key", metavar="HGE_KEY", help="admin secret key for graphql-engine", required=False
    )
    parser.addoption(
        "--hge-webhook", metavar="HGE_WEBHOOK", help="url for graphql-engine's access control webhook", required=False
    )
    parser.addoption(
        "--test-webhook-insecure", action="store_true",
        help="Run Test cases for insecure https webhook"
    )
    parser.addoption(
        "--hge-jwt-key-file", metavar="HGE_JWT_KEY_FILE", help="File containting the private key used to encode jwt tokens using RS512 algorithm", required=False
    )
    parser.addoption(
        "--hge-jwt-conf", metavar="HGE_JWT_CONF", help="The JWT conf", required=False
    )

    parser.addoption(
        "--test-cors", action="store_true",
        required=False,
        help="Run testcases for CORS configuration"
    )

    parser.addoption(
        "--test-ws-init-cookie",
        metavar="read|noread",
        required=False,
        help="Run testcases for testing cookie sending over websockets"
    )

    parser.addoption(
        "--test-metadata-disabled", action="store_true",
        help="Run Test cases with metadata queries being disabled"
    )

    parser.addoption(
        "--test-graphql-disabled", action="store_true",
        help="Run Test cases with GraphQL queries being disabled"
    )

    parser.addoption(
        "--test-hge-scale-url",
        metavar="<url>",
        required=False,
        help="Run testcases for horizontal scaling"
    )

    parser.addoption(
        "--test-allowlist-queries", action="store_true",
        help="Run Test cases with allowlist queries enabled"
    )

    parser.addoption(
        "--test-logging",
        action="store_true",
        default=False,
        required=False,
        help="Run testcases for logging"
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

#By default,
#1) Set default parallelism to one
#2) Set test grouping to by filename (--dist=loadfile)
def pytest_cmdline_preparse(config, args):
    worker = os.environ.get('PYTEST_XDIST_WORKER')
    if 'xdist' in sys.modules and not worker:  # pytest-xdist plugin
        num = 1
        args[:] = ["-n" + str(num),"--dist=loadfile"] + args

def pytest_configure(config):
    # Pytest has removed the global pytest.config
    # As a solution we are going to store it in PytestConf.config
    PytestConf.config = config
    if is_help_option_present(config):
        return
    if is_master(config):
        if not config.getoption('--hge-urls'):
            print("hge-urls should be specified")
        if not config.getoption('--pg-urls'):
            print("pg-urls should be specified")
        config.hge_url_list = config.getoption('--hge-urls')
        config.pg_url_list =  config.getoption('--pg-urls')
        config.hge_ctx_gql_server = HGECtxGQLServer(config.hge_url_list)
        if config.getoption('-n', default=None):
            xdist_threads = config.getoption('-n')
            assert xdist_threads <= len(config.hge_url_list), "Not enough hge_urls specified, Required " + str(xdist_threads) + ", got " + str(len(config.hge_url_list))
            assert xdist_threads <= len(config.pg_url_list), "Not enough pg_urls specified, Required " + str(xdist_threads) + ", got " + str(len(config.pg_url_list))

    random.seed(datetime.now())


@pytest.hookimpl()
def pytest_report_collectionfinish(config, startdir, items):
    """
    Collect server upgrade tests to the given file
    """
    tests_file = config.getoption('--collect-upgrade-tests-to-file')
    sep=''
    tests=OrderedDict()
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
    # Pytest has removed the global pytest.config
    node.slaveinput["hge-url"] = node.config.hge_url_list.pop()
    node.slaveinput["pg-url"] = node.config.pg_url_list.pop()

def pytest_unconfigure(config):
    if is_help_option_present(config):
        return
    config.hge_ctx_gql_server.teardown()

@pytest.fixture(scope='module')
def hge_ctx(request):
    config = request.config
    print("create hge_ctx")
    if is_master(config):
        hge_url = config.hge_url_list[0]
    else:
        hge_url = config.slaveinput["hge-url"]

    if is_master(config):
        pg_url = config.pg_url_list[0]
    else:
        pg_url = config.slaveinput["pg-url"]

    try:
        hge_ctx = HGECtx(hge_url, pg_url, config)
    except HGECtxError as e:
        assert False, "Error from hge_cxt: " + str(e)
        # TODO this breaks things (https://github.com/pytest-dev/pytest-xdist/issues/86)
        #      so at least make sure the real error gets printed (above)
        pytest.exit(str(e))
    yield hge_ctx  # provide the fixture value
    print("teardown hge_ctx")
    hge_ctx.teardown()
    time.sleep(1)

@pytest.fixture(scope='class')
def evts_webhook(request):
    webhook_httpd = EvtsWebhookServer(server_address=('127.0.0.1', 5592))
    web_server = threading.Thread(target=webhook_httpd.serve_forever)
    web_server.start()
    yield webhook_httpd
    webhook_httpd.shutdown()
    webhook_httpd.server_close()
    web_server.join()

@pytest.fixture(scope='module')
def actions_fixture(hge_ctx):
    pg_version = hge_ctx.pg_version
    if pg_version < 100000: # version less than 10.0
        pytest.skip('Actions are not supported on Postgres version < 10')

    # Start actions' webhook server
    webhook_httpd = ActionsWebhookServer(hge_ctx, server_address=('127.0.0.1', 5593))
    web_server = threading.Thread(target=webhook_httpd.serve_forever)
    web_server.start()
    yield webhook_httpd
    webhook_httpd.shutdown()
    webhook_httpd.server_close()
    web_server.join()

@pytest.fixture(scope='class')
def scheduled_triggers_evts_webhook(request):
    webhook_httpd = EvtsWebhookServer(server_address=('127.0.0.1', 5594))
    web_server = threading.Thread(target=webhook_httpd.serve_forever)
    web_server.start()
    yield webhook_httpd
    webhook_httpd.shutdown()
    webhook_httpd.server_close()
    web_server.join()

@pytest.fixture(scope='class')
def gql_server(request, hge_ctx):
    server = HGECtxGQLServer(request.config.getoption('--pg-urls'), 5991)
    yield server
    server.teardown()


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
def per_class_tests_db_state(request, hge_ctx):
    """
    Set up the database state for select queries.
    Has a class level scope, since select queries does not change database state
    Expects either `dir()` method which provides the directory
    with `setup.yaml` and `teardown.yaml` files
    Or class variables `setup_files` and `teardown_files` that provides
    the list of setup and teardown files respectively
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
    yield from db_context_with_schema_common(
        request, hge_ctx, 'schema_setup_files', 'schema_setup.yaml', 'schema_teardown_files', 'schema_teardown.yaml', True
    )

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
    yield from db_context_common(
        request, hge_ctx, 'values_setup_files', 'values_setup.yaml',
        'values_teardown_files', 'values_teardown.yaml',
        False, False, False
    )

def db_state_context(request, hge_ctx):
    yield from db_context_with_schema_common(
        request, hge_ctx, 'setup_files', 'setup.yaml', 'teardown_files',
        'teardown.yaml', True
    )

def db_context_with_schema_common(
    request, hge_ctx, setup_files_attr, setup_default_file,
    teardown_files_attr, teardown_default_file, check_file_exists=True):
    (skip_setup, skip_teardown) = [
        request.config.getoption('--' + x)
        for x in ['skip-schema-setup', 'skip-schema-teardown']
    ]
    yield from db_context_common(
        request, hge_ctx, setup_files_attr, setup_default_file,
        teardown_files_attr, teardown_default_file,
        check_file_exists, skip_setup, skip_teardown
    )

def db_context_common(
        request, hge_ctx, setup_files_attr, setup_default_file,
        teardown_files_attr, teardown_default_file,
        check_file_exists=True, skip_setup=True, skip_teardown=True ):
    def get_files(attr, default_file):
        files = getattr(request.cls, attr, None)
        if not files:
            files = os.path.join(request.cls.dir(), default_file)
        return files
    setup = get_files(setup_files_attr, setup_default_file)
    teardown = get_files(teardown_files_attr, teardown_default_file)
    yield from setup_and_teardown(request, hge_ctx, setup, teardown, check_file_exists, skip_setup, skip_teardown)

def setup_and_teardown(request, hge_ctx, setup_files, teardown_files, check_file_exists=True, skip_setup=False, skip_teardown=False):
    def assert_file_exists(f):
        assert os.path.isfile(f), 'Could not find file ' + f
    if check_file_exists:
        for o in [setup_files, teardown_files]:
            run_on_elem_or_list(assert_file_exists, o)
    def v1q_f(f):
        if os.path.isfile(f):
            st_code, resp = hge_ctx.v1q_f(f)
            assert st_code == 200, resp
    if not skip_setup:
        run_on_elem_or_list(v1q_f, setup_files)
    yield
    # Teardown anyway if any of the tests have failed
    if request.session.testsfailed > 0 or not skip_teardown:
        run_on_elem_or_list(v1q_f, teardown_files)

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
    return not hasattr(config, 'slaveinput')
