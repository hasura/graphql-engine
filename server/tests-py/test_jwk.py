import requests
import pytest
from context import PytestConf

if not PytestConf.config.getoption("--test-jwk-url"):
    pytest.skip("--test-jwk-url flag is missing, skipping tests", allow_module_level=True)

# assumes the JWK server is running on 127.0.0.1:5001

def test_cache_control_header(hge_ctx):
    print(hge_ctx)
    resp = requests.get('http://localhost:5001/state')
    state = resp.json()
    print(state)
    assert(state['cache-control'] > 0)

def test_expires_header(hge_ctx):
    print(hge_ctx)
    resp = requests.get('http://localhost:5001/state')
    state = resp.json()
    print(state)
    assert(state['expires'] > 0)
