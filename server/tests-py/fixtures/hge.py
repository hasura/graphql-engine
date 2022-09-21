import os
import pytest
import subprocess
import threading
from typing import Optional

import ports

# These are the names of the environment variables that should be passed through to the HGE binary.
# Other variables are ignored.
_USED_ENV_VARS = set([
    'PATH',  # required for basically anything to work
    'HASURA_GRAPHQL_PG_SOURCE_URL_1',
    'HASURA_GRAPHQL_PG_SOURCE_URL_2',
    'EVENT_WEBHOOK_HEADER',
    'EVENT_WEBHOOK_HANDLER',
    'ACTION_WEBHOOK_HANDLER',
    'SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN',
    'REMOTE_SCHEMAS_WEBHOOK_DOMAIN',
    'GRAPHQL_SERVICE_HANDLER',
    'GRAPHQL_SERVICE_1',
    'GRAPHQL_SERVICE_2',
    'GRAPHQL_SERVICE_3',
])

def hge_port() -> int:
    return ports.find_free_port()

def hge_server(
    request: pytest.FixtureRequest,
    hge_port: int,
    pg_url: str,
) -> Optional[str]:
    hge_url = f'http://localhost:{hge_port}'
    hge_bin: str = request.config.getoption('--hge-bin')  # type: ignore
    if not hge_bin:
      return None
    hge_env = {name: value for name, value in os.environ.items() if name in _USED_ENV_VARS}

    print(f'Starting GraphQL Engine on {hge_url}...')
    hge_process = subprocess.Popen(
        args = [
            hge_bin,
            '--database-url', pg_url,
            'serve',
            '--server-port', str(hge_port),
            '--stringify-numeric-types',
        ],
        env = hge_env,
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

    ports.wait_for_port(hge_port, timeout = 30)
    return hge_url
