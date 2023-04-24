import pytest
import sqlalchemy

from context import HGECtx
import fixtures.postgres
from validate import check_query_f

usefixtures = pytest.mark.usefixtures

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
        if transport != 'http':
            pytest.skip('This test should only run over HTTP.')
        check_query_f(hge_ctx, self.dir() + '/update_query_aves.yaml', transport)


# As this is a read-only test, we can't create the schema/tables as part of the
# HGE metadata. Hence, we create it as a separate fixture, where we execute the
# DDLs directly on the database.
@pytest.fixture(scope='class')
def setup_schema_externally(
    owner_engine: sqlalchemy.engine.Engine,
    hge_ctx: HGECtx,
    add_source,
):
    source = 'read_only'
    backend = add_source(source, read_only = True)

    with fixtures.postgres.switch_schema(owner_engine, backend.name).connect() as connection:
        connection.execute("CREATE TABLE aves (id serial PRIMARY KEY, name TEXT)")
        connection.execute("INSERT INTO aves (name) VALUES ('Booted Eagle'), ('Hooded Merganser')")

    hge_ctx.v1metadataq({
        'type': 'pg_track_table',
        'args': {
            'source': source,
            'table': {
                'name': 'aves',
            },
        },
    })
