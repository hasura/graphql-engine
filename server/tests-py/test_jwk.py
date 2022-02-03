from time import sleep
import requests
import pytest
from context import PytestConf

if not PytestConf.config.getoption("--test-jwk-url"):
    pytest.skip("--test-jwk-url flag is missing, skipping tests", allow_module_level=True)

# assumes the JWK server is running on 127.0.0.1:5001

def test_cache_control_header_max_age(hge_ctx):
    requests.post('http://localhost:5001/reset-state')
    sleep(3)
    resp = requests.get('http://localhost:5001/state')
    state = resp.json()
    print(state)
    # The test uses max-age=3 so we should only see one refresh
    assert(state['cache-control'] == 1)

def test_cache_control_header_no_caching(hge_ctx):
    requests.post('http://localhost:5001/reset-state')
    sleep(3)
    resp = requests.get('http://localhost:5001/state')
    state = resp.json()
    print(state)
    # HGE should refresh the JWK once a second, so, depending on timing we might get 2-3 refreshes performed
    assert(state['cache-control'] >= 2 and state['cache-control'] <= 3)

def test_expires_header(hge_ctx):
    requests.post('http://localhost:5001/reset-state')
    sleep(3)
    resp = requests.get('http://localhost:5001/state')
    state = resp.json()
    print(state)
    # The test uses a three second expiry so we should only see one refresh
    assert(state['expires'] == 1)
