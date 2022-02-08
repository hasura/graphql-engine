from time import sleep, perf_counter
import requests
import pytest
from context import PytestConf

if not PytestConf.config.getoption("--test-jwk-url"):
    pytest.skip("--test-jwk-url flag is missing, skipping tests", allow_module_level=True)

# assumes the JWK server is running on 127.0.0.1:5001

def wait_until_request_count_reaches(num_requests, state_key, max_wait_secs):
    start_time = perf_counter()
    requests.post('http://localhost:5001/reset-state')
    request_count = 0
    time_elapsed = 0

    while request_count < num_requests:
        time_elapsed = perf_counter() - start_time
        if time_elapsed > max_wait_secs:
            raise Exception(f'Waited {time_elapsed} seconds for {state_key} JWK requests to reach {num_requests}. Only received {request_count}.')

        sleep(0.2)
        state = requests.get('http://localhost:5001/state').json()
        request_count = state[state_key]

    return time_elapsed

def test_cache_control_header_max_age(hge_ctx):
    # The test uses max-age=3, so we are expecting one request (timing out after 6 seconds)
    time_elapsed = wait_until_request_count_reaches(1, 'cache-control', 6)
    print(f"time_elapsed: {time_elapsed}")

def test_cache_control_header_no_caching(hge_ctx):
    # HGE should refresh the JWK once a second, so we are expecting three requests in at least two seconds
    # (timing out after 10 seconds)
    time_elapsed = wait_until_request_count_reaches(3, 'cache-control', 10)
    print(f"time_elapsed: {time_elapsed}")
    assert(time_elapsed >= 2)

def test_expires_header(hge_ctx):
    # The test uses a three second jwk expiry so we are expecting one request (timing out after 6 seconds)
    time_elapsed = wait_until_request_count_reaches(1, 'expires', 6)
    print(f"time_elapsed: {time_elapsed}")
