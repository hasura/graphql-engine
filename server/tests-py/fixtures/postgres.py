import pytest
import re
import sqlalchemy
import threading
import time
from typing import Any, Optional
import urllib.parse
import zlib

from context import HGECtx


class Backend:
    def __init__(self, name: str, engine: sqlalchemy.engine.Engine):
        self.name = name
        self.url = str(engine.url)
        self.engine = engine
        self.kind = 'postgres'

    def __repr__(self):
        return f'{self.__class__.__name__}(name={self.name!r}, url={self.url!r})'

    @property
    def connection_parameters(self):
        parsed = urllib.parse.urlparse(self.url)
        return {
            "host": parsed.hostname,
            "username": parsed.username,
            "port": parsed.port,
            "database": re.sub('^/', '', parsed.path),
        }


# Switch to a new schema using the same connection details as the given engine.
def switch_schema(engine: sqlalchemy.engine.Engine, new_schema: str):
    parsed_url = urllib.parse.urlparse(str(engine.url))
    new_url = urllib.parse.urlunparse(parsed_url._replace(path = f'/{new_schema}'))
    return sqlalchemy.engine.create_engine(new_url)


# Acquire a single SQLAlchemy engine for the entire session.
def owner_engine(request: pytest.FixtureRequest) -> sqlalchemy.engine.Engine:
    pg_url: str = request.config.getoption('--pg-urls')[0]  # type: ignore
    return sqlalchemy.engine.create_engine(pg_url)


# Create a new user to run the tests. This user is *not* a superuser.
# Otherwise we cannot remove database permissions from HGE.
def runner_engine(owner_engine: sqlalchemy.engine.Engine) -> sqlalchemy.engine.Engine:
    user_name = 'hasura_server_tests_py'

    with owner_engine.connect() as connection:
        # Effectively, this emulates `CREATE USER IF NOT EXISTS`.
        try:
            connection.execute(f"CREATE USER {user_name} WITH PASSWORD 'password'")
        except:
            pass

    parsed_pg_url = urllib.parse.urlparse(str(owner_engine.url))
    # Create a new URL with the new username, and the database set to "postgres".
    # This is fine for connecting; you can't do anything with it.
    test_pg_url = urllib.parse.urlunparse(parsed_pg_url._replace(netloc = re.sub('^.+@', f'{user_name}:password@', parsed_pg_url.netloc), path = '/postgres'))

    return sqlalchemy.engine.create_engine(test_pg_url)


def create_schema(
    request: pytest.FixtureRequest,
    owner_engine: sqlalchemy.engine.Engine,
    runner_engine: sqlalchemy.engine.Engine,
    prefix: str,
    read_only: bool = False,
) -> Backend:
    # generate a database from the fully-qualified test name
    # e.g. for the test, 'test_metadata.py::TestMetadata::test_reload_metadata',
    # the name will be 'tests_py_test_metadata_py_testmetadata_test_reload_metadata'
    name = '_'.join(re.sub('[\\.\\-]', '_', node.name.lower()) for node in request.node.listchain())
    schema_name = f'test_{prefix}_{name}'
    # PostgreSQL truncates database names to 63 characters.
    if len(schema_name) >= 64:
        # use a quick, short, insecure hash
        hash = zlib.adler32(name.encode('ascii'))
        schema_name = f'test_{prefix}_{hash:x}'

    drop_schema(owner_engine, schema_name)

    with owner_engine.connect() as connection:
        connection.execute('COMMIT')  # Exit the implicit transaction.
        connection.execute(f'CREATE DATABASE {schema_name}')
        request.addfinalizer(lambda: drop_schema_in_background(owner_engine, schema_name))

    with switch_schema(owner_engine, schema_name).connect() as connection:
        # We need to install extensions for PostgreSQL <= v12. From v13 onwards,
        # extensions can be installed by non-superusers, and the following can
        # be removed.

        # Required as otherwise, some tests will try to use HGE to install them,
        # but HGE does not have superuser privileges. Ideally, the tests would
        # use a different mechanism to install the extensions, but that's a lot
        # more work.
        connection.execute('CREATE EXTENSION IF NOT EXISTS citext')
        connection.execute('CREATE EXTENSION IF NOT EXISTS ltree')
        # Required as otherwise, HGE will try to install it itself, but does not
        # have superuser privileges.
        connection.execute('CREATE EXTENSION IF NOT EXISTS pgcrypto')

        # Some tests use the "hge_tests" schema.
        connection.execute('CREATE SCHEMA hge_tests')

        # If the test requires a read-only source, revoke all write privileges.
        if read_only:
            connection.execute(f'REVOKE ALL PRIVILEGES ON SCHEMA public FROM public')
            connection.execute(f'REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA public FROM public')
            connection.execute(f'GRANT USAGE ON SCHEMA public TO public')
            connection.execute(f'GRANT SELECT ON ALL TABLES IN SCHEMA public TO public')
            connection.execute(f'GRANT SELECT ON ALL TABLES IN SCHEMA pg_catalog TO public')
            connection.execute(f'GRANT SELECT ON ALL TABLES IN SCHEMA information_schema TO public')
            connection.execute(f'ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO public')
        # Otherwise, grant all privileges.
        else:
            connection.execute(f'GRANT ALL PRIVILEGES ON DATABASE {schema_name} TO {runner_engine.url.username}')
            connection.execute(f'GRANT ALL PRIVILEGES ON SCHEMA public TO {runner_engine.url.username}')
            connection.execute(f'GRANT ALL PRIVILEGES ON SCHEMA hge_tests TO {runner_engine.url.username}')

    engine = switch_schema(runner_engine, schema_name)
    return Backend(name = schema_name, engine = engine)


# Dropping the database can be tricky because we cannot guarantee fixture shutdown order.
# Perhaps the GraphQL Engine is still using it.
# To avoid this, we try a few times in a background thread, allowing other cleanup to continue.
def drop_schema_in_background(engine: sqlalchemy.engine.Engine, name: str):
    t = threading.Thread(target = lambda: retry(f'DROP DATABASE {name}', lambda: drop_schema(engine, name), tries = 3))
    t.start()


def drop_schema(engine: sqlalchemy.engine.Engine, name: str):
    with engine.connect() as connection:
        connection.execute('COMMIT')  # Exit the implicit transaction.
        connection.execute(f'DROP DATABASE IF EXISTS {name}')


def retry(message, f, tries = 1, delay = 3):
    try:
        print(f'Attempting to {message}...')
        f()
    except:
        if tries == 1:
            print(f'Failed to {message}. Giving up.')
        else:
            print(f'Failed to {message}. Waiting {delay} seconds then trying again.')
            time.sleep(delay)
            retry(message, f, tries = tries - 1)


def metadata_schema_url(
    request: pytest.FixtureRequest,
    owner_engine: sqlalchemy.engine.Engine,
    runner_engine: sqlalchemy.engine.Engine,
):
    return create_schema(request, owner_engine, runner_engine, 'metadata').url


def new_source(
    request: pytest.FixtureRequest,
    owner_engine: sqlalchemy.engine.Engine,
    runner_engine: sqlalchemy.engine.Engine,
    hge_ctx: HGECtx,
    name: Optional[str] = None,
    customization: Any = {},
):
    source_name = name or 'default'
    prefix = f'source_{source_name}'
    backend = create_schema(request, owner_engine, runner_engine, prefix)
    add_source(hge_ctx, backend, source_name, customization)

    yield backend

    drop_source(hge_ctx, source_name)


def add_source(hge_ctx: HGECtx, backend: Backend, source_name: str, customization: Any = {}):
    metadata = hge_ctx.v1metadataq({
        'type': 'export_metadata',
        'args': {},
    })
    metadata['sources'].append({
        'name': source_name or 'default',
        'kind': backend.kind,
        'configuration': {
            'connection_info': {
                'database_url': backend.url,
            },
        },
        'customization': customization,
        'tables': [],
    })
    hge_ctx.v1metadataq({
        'type': 'replace_metadata',
        'args': metadata,
    })


def drop_source(hge_ctx: HGECtx, source_name: str):
    metadata = hge_ctx.v1metadataq({
        'type': 'export_metadata',
        'args': {},
    })
    metadata['sources'] = [source for source in metadata['sources'] if source['name'] != source_name]
    hge_ctx.v1metadataq({
        'type': 'replace_metadata',
        'args': metadata,
    })


def source_backend(
    request: pytest.FixtureRequest,
    owner_engine: sqlalchemy.engine.Engine,
    runner_engine: sqlalchemy.engine.Engine,
    hge_ctx: HGECtx,
    name: Optional[str] = None,
):
    disabled_marker = request.node.get_closest_marker('default_source_disabled')
    if disabled_marker:
        yield None
    else:
        yield from new_source(request, owner_engine, runner_engine, hge_ctx, name)


def postgis(owner_engine: sqlalchemy.engine.Engine, source_backend: Optional[Backend]):
    # TODO: remove once parallelization work is completed
    #       `source_backend` will no longer be optional
    engine = switch_schema(owner_engine, source_backend.name) if source_backend else owner_engine
    with engine.connect() as connection:
        connection.execute('CREATE EXTENSION IF NOT EXISTS postgis')
        connection.execute('CREATE EXTENSION IF NOT EXISTS postgis_topology')
        result = connection.execute('SELECT PostGIS_lib_version() as postgis_version').fetchone()
        if not result:
            raise Exception('Could not detect the PostGIS version.')
        postgis_version: str = result['postgis_version']
        if re.match('^3\\.', postgis_version):
            connection.execute('CREATE EXTENSION IF NOT EXISTS postgis_raster')
