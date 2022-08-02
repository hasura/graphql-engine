import pytest
import psycopg2
from validate import check_query_f
from context import PytestConf

# Mark that all tests in this module can be run as server upgrade tests
pytestmark = pytest.mark.allow_server_upgrade_test

usefixtures = pytest.mark.usefixtures

if not PytestConf.config.getoption('--test-read-only-source'):
    pytest.skip('--test-read-only-source flag is missing, skipping read-only tests',
                allow_module_level=True)

@pytest.mark.parametrize('transport', ['http', 'websocket'])
@pytest.mark.backend('postgres', 'citus')
#@pytest.mark.backend('citus', 'mssql', 'postgres')
@usefixtures('setup_schema_externally', 'per_class_tests_db_state')
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
def setup_schema_externally(hge_ctx):
    if hge_ctx.backend in ['postgres', 'citus']:
        conn = setup_postgres_schema(hge_ctx.pg_url)
        yield conn
        teardown_postgres_schema(conn)
    elif hge_ctx.backend == 'mssql':
        # TODO: will this be pg_url?
        setup_mssql_schema(hge_ctx.pg_url)
    else:
        raise Exception('setup_schema_externally fixture was used with an unknown backend')

def setup_postgres_schema(conn_url):
    conn = psycopg2.connect(conn_url)
    cur = conn.cursor()
    cur.execute("CREATE TABLE aves (id serial PRIMARY KEY, name TEXT);")
    cur.execute("INSERT INTO aves (name) VALUES ('Booted Eagle'), ('Hooded Merganser');")
    conn.commit()
    return conn

def teardown_postgres_schema(conn):
    cur = conn.cursor()
    cur.execute("DROP TABLE aves;")
    conn.commit()
    cur.close()
    conn.close()

def setup_mssql_schema(conn_url):
    pass
