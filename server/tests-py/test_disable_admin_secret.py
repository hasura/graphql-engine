import os
import subprocess

import pytest

import ports


def test_disable_admin_secret_requires_fallback_auth(
    hge_bin, metadata_schema_url, worker_id
):
    if not hge_bin:
        pytest.skip('This test requires --hge-bin')

    port = ports.find_free_port(worker_id)
    env = {
        **os.environ,
        'HASURA_GRAPHQL_DISABLE_ADMIN_SECRET': 'true',
    }
    for name in (
        'HASURA_GRAPHQL_AUTH_HOOK',
        'HASURA_GRAPHQL_JWT_SECRET',
        'HASURA_GRAPHQL_JWT_SECRETS',
        'HASURA_GRAPHQL_SSO_PROVIDERS',
    ):
        env.pop(name, None)

    result = subprocess.run(
        [
            hge_bin,
            '--metadata-database-url',
            metadata_schema_url,
            'serve',
            '--server-port',
            str(port),
            '--disable-admin-secret',
        ],
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        encoding='utf-8',
        timeout=30,
    )

    assert result.returncode != 0
    assert (
        'Fatal Error: requires either HASURA_GRAPHQL_AUTH_HOOK'
    ) in result.stdout
