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
    'HASURA_GRAPHQL_CITUS_SOURCE_URL',
    'HASURA_GRAPHQL_MSSQL_SOURCE_URL',

    # required for Nix-based ODBC driver configuration
    'ODBCSYSINI',
    'ODBCINSTINI',
])


def hge_port(worker_id: str) -> int:
    return ports.find_free_port(worker_id)


def hge_server(
    request: pytest.FixtureRequest,
    hge_bin: str,
    hge_port: int,
    hge_url: str,
    hge_key: Optional[str],
    hge_fixture_env: dict[str, str],
    metadata_schema_url: str,
) -> subprocess.Popen[bytes]:
    hge_env: dict[str, str] = {name: value for name, value in os.environ.items() if name in _PASS_THROUGH_ENV_VARS}
    hge_marker_env: dict[str, str] = {marker.args[0]: marker.args[1] for marker in request.node.iter_markers('hge_env') if marker.args[1] is not None}
    env = {
        **hge_env,
        **hge_fixture_env,
        **hge_marker_env,
    }

    hge_key_args = ['--admin-secret', hge_key] if hge_key else []

    if request.node.get_closest_marker('capture_hge_logs'):
        # capture the logs
        stdout = subprocess.PIPE
        stderr = subprocess.STDOUT # combine with stdout
    else:
        # just stream outwards, so that the user can see the logs on a failing test
        stdout = None
        stderr = None

    print(f'Starting GraphQL Engine on {hge_url}...')
    hge_process = subprocess.Popen(
        args = [
            hge_bin,
            '--metadata-database-url', metadata_schema_url,
            'serve',
            '--server-port', str(hge_port),
            '--stringify-numeric-types',
            *hge_key_args,
        ],
        env = env,
        stdout = stdout,
        stderr = stderr,
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

    # Stop in the background so we don't hold up other tests.
    request.addfinalizer(lambda: threading.Thread(target = stop).start())

    try:
        ports.wait_for_port(hge_port, timeout = 30)
    except TimeoutError:
        # print the logs so we can diagnose the issue
        if hge_process.stdout:
            print(hge_process.stdout.read())
        raise

    return hge_process
