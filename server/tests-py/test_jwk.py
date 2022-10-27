import os
import pytest
import requests
import threading
from time import perf_counter, sleep
from conftest import extract_server_address_from

import jwk_server
import ports

pytestmark = [
    pytest.mark.admin_secret
]

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
