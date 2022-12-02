import os
import pytest
import sqlalchemy
from typing import Optional
import urllib.parse

from context import HGECtx, PytestConf
import fixtures.postgres
from validate import check_query_f

# Mark that all tests in this module can be run as server upgrade tests
pytestmark = pytest.mark.allow_server_upgrade_test

usefixtures = pytest.mark.usefixtures

if not PytestConf.config.getoption('--test-read-only-source'):
    pytest.skip('--test-read-only-source flag is missing, skipping read-only tests',
                allow_module_level=True)

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@pytest.mark.backend('postgres', 'citus')
@pytest.mark.admin_secret
@pytest.mark.hge_env('HASURA_GRAPHQL_ADMIN_INTERNAL_ERRORS', 'false')
@usefixtures('setup_schema_externally')
class TestGraphQLOnReadOnlySource:

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/read_only_source'

    setup_metadata_api_version = 'v2'

    def test_query_aves(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/select_query_aves.yaml', transport)

    # graphql-engine's websocket response is different than in http on execution
    # errors; so this test is run only on http
    def test_mutation_aves(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/update_query_aves.yaml', 'http')


# As this is a read-only test, we can't create the schema/tables as part of the
# HGE metadata. Hence, we create it as a separate fixture, where we execute the
# DDLs directly on the database.
@pytest.fixture(scope='class')
def setup_schema_externally(
    owner_engine: sqlalchemy.engine.Engine,
    hge_ctx: HGECtx,
    source_backend: Optional[fixtures.postgres.Backend],
):
    # TODO: remove once parallelization work is completed
    #       just use the default source, and get rid of this environment variable entirely
    readonly_db_url = os.getenv('HASURA_READONLY_DB_URL')

    if source_backend:
        engine = source_backend.engine
    elif readonly_db_url:
        engine = fixtures.postgres.switch_schema(owner_engine, urllib.parse.urlparse(readonly_db_url).path.lstrip('/'))
    else:
        raise Exception('No database available.')

    source = 'default'
    # TODO: remove once parallelization work is completed
    if readonly_db_url:
        source = 'pg_readonly'
        hge_ctx.v1metadataq({
            'type': 'pg_add_source',
            'args': {
                'name': source,
                'configuration': {
                    'connection_info': {
                        'database_url': {
                            'from_env': 'HASURA_READONLY_DB_URL',
                        },
                    },
                },
            },
        })

    with engine.connect() as connection:
        connection.execute("CREATE TABLE aves (id serial PRIMARY KEY, name TEXT)")
        connection.execute("INSERT INTO aves (name) VALUES ('Booted Eagle'), ('Hooded Merganser')")

    # TODO: remove once parallelization work is completed
    #       `source_backend` will no longer be optional
    if source_backend:
        # Revoke all write privileges from the given user
        with fixtures.postgres.switch_schema(owner_engine, source_backend.name).connect() as connection:
            username = source_backend.engine.url.username
            connection.execute(f'REVOKE ALL PRIVILEGES ON SCHEMA public FROM {username}')
            connection.execute(f'REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA public FROM {username}')
            connection.execute(f'GRANT USAGE ON SCHEMA public TO {username}')
            connection.execute(f'GRANT SELECT ON ALL TABLES IN SCHEMA public TO {username}')
            connection.execute(f'GRANT SELECT ON ALL TABLES IN SCHEMA pg_catalog TO {username}')
            connection.execute(f'GRANT SELECT ON ALL TABLES IN SCHEMA information_schema TO {username}')
            connection.execute(f'ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO {username}')

    hge_ctx.v1metadataq({
        'type': 'pg_track_table',
        'args': {
            'source': source,
            'table': {
                'name': 'aves',
            },
        },
    })

    yield

    # TODO: remove once parallelization work is completed
    if readonly_db_url:
        hge_ctx.v1metadataq({
            'type': 'pg_drop_source',
            'args': {
                'name': source,
            },
        })

    # TODO: remove once parallelization work is completed
    #       we can just drop the whole database
    with engine.connect() as connection:
        connection.execute('DROP TABLE aves')
