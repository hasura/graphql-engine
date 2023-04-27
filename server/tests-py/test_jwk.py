import pytest
import requests
from time import perf_counter, sleep

pytestmark = [
    pytest.mark.admin_secret
]

def wait_until_request_count_reaches(num_requests: int, state_key: str, timeout_secs: int, jwk_server_url: str) -> float:
    start_time = perf_counter()
    requests.post(jwk_server_url + '/reset-state')
    request_count = 0
    time_elapsed = 0

    while request_count < num_requests:
        time_elapsed = perf_counter() - start_time
        if time_elapsed > timeout_secs:
            raise Exception(f'Waited {time_elapsed} seconds for {state_key} JWK requests to reach {num_requests}. Only received {request_count}.')

        sleep(0.2)
        state = requests.get(jwk_server_url + '/state').json()
        request_count = state[state_key]

    return time_elapsed

@pytest.mark.jwk_path('/jwk-cache-control?max-age=3')
def test_cache_control_header_max_age(jwk_server_url: str):
    # The test uses max-age=3, so we are expecting one request
    time_elapsed = wait_until_request_count_reaches(num_requests=1, state_key='cache-control', timeout_secs=6, jwk_server_url=jwk_server_url)
    print(f"time_elapsed: {time_elapsed}")

@pytest.mark.jwk_path('/jwk-cache-control?max-age=3&must-revalidate=true')
def test_cache_control_header_max_age_must_revalidate(jwk_server_url: str):
    # The test uses max-age=3, so we are expecting one request
    time_elapsed = wait_until_request_count_reaches(num_requests=1, state_key='cache-control', timeout_secs=6, jwk_server_url=jwk_server_url)
    print(f"time_elapsed: {time_elapsed}")

@pytest.mark.jwk_path('/jwk-cache-control?must-revalidate=true')
def test_cache_control_header_must_revalidate(jwk_server_url: str):
    # HGE should refresh the JWK once a second, so we are expecting three requests in at least two seconds
    time_elapsed = wait_until_request_count_reaches(num_requests=3, state_key='cache-control', timeout_secs=10, jwk_server_url=jwk_server_url)
    print(f"time_elapsed: {time_elapsed}")
    assert(time_elapsed >= 2)

@pytest.mark.jwk_path('/jwk-cache-control?no-cache=true&public=true')
def test_cache_control_header_no_cache_public(jwk_server_url: str):
    # HGE should refresh the JWK once a second, so we are expecting three requests in at least two seconds
    time_elapsed = wait_until_request_count_reaches(num_requests=3, state_key='cache-control', timeout_secs=10, jwk_server_url=jwk_server_url)
    print(f"time_elapsed: {time_elapsed}")
    assert(time_elapsed >= 2)

@pytest.mark.jwk_path('/jwk-cache-control?no-store=true&max-age=3')
def test_cache_control_header_no_store_max_age(jwk_server_url: str):
    # HGE should refresh the JWK once a second, so we are expecting three requests in at least two seconds
    time_elapsed = wait_until_request_count_reaches(num_requests=3, state_key='cache-control', timeout_secs=10, jwk_server_url=jwk_server_url)
    print(f"time_elapsed: {time_elapsed}")
    assert(time_elapsed >= 2)

@pytest.mark.jwk_path('/jwk-expires?seconds=3')
def test_expires_header(jwk_server_url: str):
    # The test uses a three second jwk expiry so we are expecting one request
    time_elapsed = wait_until_request_count_reaches(num_requests=1, state_key='expires', timeout_secs=6, jwk_server_url=jwk_server_url)
    print(f"time_elapsed: {time_elapsed}")
