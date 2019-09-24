import pytest
import time
from context import HGECtx, HGECtxError, EvtsWebhookServer, HGECtxGQLServer, GQLWsClient
import threading
import random
from datetime import datetime
import sys
import os
import socket

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


#By default,
#1) Set default parallelism to one
#2) Set test grouping to by filename (--dist=loadfile)
def pytest_cmdline_preparse(config, args):
    worker = os.environ.get('PYTEST_XDIST_WORKER')
    if 'xdist' in sys.modules and not worker and 'no:xdist' not in args:  # pytest-xdist plugin
        num = 1
        args[:] = ["-n" + str(num),"--dist=loadfile"] + args


def pytest_configure(config):
    if is_master(config):
        if not config.getoption('--hge-urls'):
            print("hge-urls should be specified")
        if not config.getoption('--pg-urls'):
            print("pg-urls should be specified")
        config.hge_url_list = config.getoption('--hge-urls')
        config.pg_url_list =  config.getoption('--pg-urls')
        test_threads = config.getoption('-n', default=1)
        assert test_threads <= len(config.hge_url_list), "Not enough hge_urls specified, Required " + str(test_threads) + ", got " + str(len(config.hge_url_list))
        assert test_threads <= len(config.pg_url_list), "Not enough pg_urls specified, Required " + str(test_threads) + ", got " + str(len(config.pg_url_list))
        config.remote_gql_port_list = get_unused_ports(5000, test_threads)

    random.seed(datetime.now())
    # Reset the environment variable which is to be set only by fixture remote_gql_server
    os.environ.pop('REMOTE_GRAPHQL_ROOT_URL', None)

@pytest.hookimpl(optionalhook=True)
def pytest_configure_node(node):
    for attr in ['hge_url', 'pg_url', 'remote_gql_port']:
        node.slaveinput[attr] = getattr(node.config, attr + '_list').pop()

def get_conf(config, attr):
    if is_master(config):
        return getattr(config, attr + '_list')[0]
    else:
        return config.slaveinput[attr]

@pytest.fixture(scope='module')
def hge_ctx(request):
    config = request.config
    print("create hge_ctx")
    hge_url = get_conf(config, 'hge_url')
    pg_url = get_conf(config, 'pg_url')
    hge_key = config.getoption('--hge-key')
    hge_webhook = config.getoption('--hge-webhook')
    webhook_insecure = config.getoption('--test-webhook-insecure')
    hge_jwt_key_file = config.getoption('--hge-jwt-key-file')
    hge_jwt_conf = config.getoption('--hge-jwt-conf')
    ws_read_cookie = config.getoption('--test-ws-init-cookie')
    metadata_disabled = config.getoption('--test-metadata-disabled')
    hge_scale_url = config.getoption('--test-hge-scale-url')
    try:
        hge_ctx = HGECtx(
            hge_url = hge_url,
            pg_url = pg_url,
            hge_key = hge_key,
            hge_webhook = hge_webhook,
            webhook_insecure = webhook_insecure,
            hge_jwt_key_file = hge_jwt_key_file,
            hge_jwt_conf = hge_jwt_conf,
            ws_read_cookie = ws_read_cookie,
            metadata_disabled = metadata_disabled,
            hge_scale_url = hge_scale_url,
        )
    except HGECtxError as e:
        assert False, "Error from hge_cxt: " + str(e)
        # TODO this breaks things (https://github.com/pytest-dev/pytest-xdist/issues/86)
        #      so at least make sure the real error gets printed (above)
        pytest.exit(str(e))

    yield hge_ctx  # provide the fixture value
    print("teardown hge_ctx")
    hge_ctx.teardown()
    time.sleep(1)

@pytest.fixture(scope='module')
def remote_gql_server(request, hge_ctx):
    """Sets up the remote GraphQL server needed for tests with remote servers"""
    port = get_conf(request.config, 'remote_gql_port')

    remote_gql_server = HGECtxGQLServer(hge_ctx, '127.0.0.1', port)
    # Set environmental variable
    os.environ['REMOTE_GRAPHQL_ROOT_URL'] = remote_gql_server.root_url
    yield remote_gql_server
    remote_gql_server.teardown()
    del os.environ['REMOTE_GRAPHQL_ROOT_URL']

@pytest.fixture(scope='class')
def evts_webhook(request):
    webhook_httpd = EvtsWebhookServer(server_address=('127.0.0.1', 5592))
    web_server = start_webserver(webhook_httpd)
    yield webhook_httpd
    stop_webserver(web_server)

@pytest.fixture(scope='class')
def ws_client(request, hge_ctx):
    client = GQLWsClient(hge_ctx, '/v1/graphql')
    time.sleep(0.1)
    yield client
    client.teardown()

@pytest.fixture(scope='class')
def setup_ctrl(request, hge_ctx):
    """
    This fixure is used to store the state of test setup in some test classes.
    Used primarily when teardown is skipped in some test cases in the class where the test is not expected to change the database state.
    """
    setup_ctrl = { "setupDone" : False }
    yield setup_ctrl
    hge_ctx.may_skip_test_teardown = False
    request.cls().do_teardown(setup_ctrl, hge_ctx)

def start_webserver(httpd):
    webserver = threading.Thread(target=httpd.serve_forever)
    webserver.httpd = httpd
    webserver.start()
    return webserver

def stop_webserver(webserver):
    webserver.httpd.shutdown()
    webserver.httpd.server_close()
    webserver.join()

def get_unused_ports(start, count):
    ports = []
    for i in range(0, count):
        port = get_unused_port(start)
        ports.append(port)
        start = port + 1
    return ports

def is_port_open(port):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        res = sock.connect_ex(('127.0.0.1', port))
        return res == 0

def get_unused_port(start):
    if is_port_open(start):
        return get_unused_port(start + 1)
    else:
        return start

def is_master(config):
    """True if the code running the given pytest.config object is running in a xdist master
    node or not running xdist at all.
    """
    return not hasattr(config, 'slaveinput')
