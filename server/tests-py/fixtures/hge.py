import os
import pytest
import subprocess
import threading
from typing import Optional

import ports

# These are the names of the environment variables that should be passed through to the HGE binary.
# Other variables are ignored.
_PASS_THROUGH_ENV_VARS = set([
    'PATH',  # required for basically anything to work
    'HASURA_GRAPHQL_PG_SOURCE_URL_1',
    'HASURA_GRAPHQL_PG_SOURCE_URL_2',
])


def hge_port() -> int:
    return ports.find_free_port()


def hge_server(
    request: pytest.FixtureRequest,
    hge_bin: str,
    hge_port: int,
    hge_url: str,
    hge_fixture_env: dict[str, str],
    pg_url: str,
) -> Optional[str]:
    hge_env: dict[str, str] = {name: value for name, value in os.environ.items() if name in _PASS_THROUGH_ENV_VARS}
    hge_marker_env: dict[str, str] = {marker.args[0]: marker.args[1] for marker in request.node.iter_markers('hge_env') if marker.args[1] is not None}
    env = {
        **hge_env,
        **hge_fixture_env,
        **hge_marker_env,
    }

    print(f'Starting GraphQL Engine on {hge_url}...')
    hge_process = subprocess.Popen(
        args = [
            hge_bin,
            '--database-url', pg_url,
            'serve',
            '--server-port', str(hge_port),
            '--stringify-numeric-types',
        ],
        env = env,
    )

    def stop():
        if hge_process.poll() is None:
            print(f'Stopping GraphQL Engine on {hge_url}...')
            hge_process.terminate()
            try:
                hge_process.wait(timeout = 5)
                print(f'GraphQL Engine on {hge_url} has stopped.')
            except subprocess.TimeoutExpired:
                print(f'Given up waiting; killing GraphQL Engine...')
                hge_process.kill()
                hge_process.wait()
                print(f'GraphQL Engine has been successfully killed.')
        else:
            print(f'GraphQL Engine on {hge_url} has already stopped.')

    # We can re-enable stopping in the background once tests no longer share a database.
    # Until then, HGE can interfere with other tests.
    request.addfinalizer(stop)
    ## Stop in the background so we don't hold up other tests.
    # request.addfinalizer(lambda: threading.Thread(target = stop).start())

    ports.wait_for_port(hge_port, timeout = 30)
    return hge_url
