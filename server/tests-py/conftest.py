import pytest
import time
from context import HGECtx, HGECtxError

def pytest_addoption(parser):
    parser.addoption(
        "--hge-url", metavar="HGE_URL", help="url for graphql-engine", required=True
    )
    parser.addoption(
        "--pg-url", metavar="PG_URL", help="url for connecting to Postgres directly", required=True
    )
    parser.addoption(
        "--hge-key", metavar="PG_URL", help="access key for graphql-engine", required=False
    )
    parser.addoption(
        "--hge-webhook", metavar="PG_URL", help="url for graphql-engine's access control webhook", required=False
    )
    parser.addoption(
        "--test-webhook-insecure", action="store_true",
        help="Run Test cases for insecure https webhook")


@pytest.fixture(scope='session')
def hge_ctx(request):
    print ("create hge_ctx")
    hge_url = request.config.getoption('--hge-url')
    pg_url = request.config.getoption('--pg-url')
    hge_key = request.config.getoption('--hge-key')
    hge_webhook = request.config.getoption('--hge-webhook')
    webhook_insecure = request.config.getoption('--hge-webhook')
    try:
        hge_ctx = HGECtx(hge_url=hge_url, pg_url=pg_url, hge_key=hge_key, hge_webhook=hge_webhook, webhook_insecure = webhook_insecure )
    except HGECtxError as e:
        pytest.exit(str(e))
    yield hge_ctx  # provide the fixture value
    print("teardown hge_ctx")
    hge_ctx.teardown()
    time.sleep(2)
