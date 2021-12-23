import argparse
from zipfile import ZipFile
import os
from contextlib import contextmanager
import threading
import requests
import requests_cache
from colorama import Fore, Style

from port_allocator import PortAllocator
from run_postgres import Postgres
from run_hge import HGE


def _first_true(iterable, default=False, pred=None):
    return next(filter(pred, iterable), default)


class HGETestSetup:

    sportsdb_url='http://www.sportsdb.org/modules/sd/assets/downloads/sportsdb_sample_postgresql.zip'

    default_work_dir = 'test_output'

    previous_work_dir_file = '.previous_work_dir'

    def __init__(self, pg_url, remote_pg_url, pg_docker_image, hge_url, remote_hge_url, hge_docker_image=None, hge_args=[], skip_remote_graphql_setup=False, skip_stack_build=False):
        self.pg_url = pg_url
        self.remote_pg_url = remote_pg_url
        self.pg_docker_image = pg_docker_image
        self.hge_url = hge_url
        self.remote_hge_url = remote_hge_url
        self.hge_docker_image = hge_docker_image
        self.hge_args = hge_args
        self.skip_remote_graphql_setup = skip_remote_graphql_setup
        self.skip_stack_build = skip_stack_build
        self.port_allocator = PortAllocator()
        self.init_work_dir()
        self.init_pgs()
        self.init_hges()
        self.set_previous_work_dir()

    def get_previous_work_dir(self):
        """Get the work directory of the previous error"""
        try:
            with open(self.previous_work_dir_file) as f:
                return f.read()
        except FileNotFoundError:
            return None

    def set_previous_work_dir(self):
        """
        Set the current work directory as previous work directory
        This directory will be used as the default work directory choice
        """
        with open(self.previous_work_dir_file, 'w') as f:
            return f.write(self.work_dir)

    def get_work_dir(self):
        """
        Get the work directory from environmental variable, or from the user input
        """
        default_work_dir = self.get_previous_work_dir() or self.default_work_dir
        return os.environ.get('WORK_DIR') \
            or input(Fore.YELLOW + '(Set WORK_DIR environmental variable to avoid this)\n'
                     + 'Please specify the work directory. (default:{}):'.format(default_work_dir)
                     + Style.RESET_ALL).strip() \
            or default_work_dir

    def init_work_dir(self):
        """
        Get the current work directory from user input or environmental variables
        and create the work directory if it is not present
        """
        self.work_dir = self.get_work_dir()
        print ("WORK_DIR: ", self.work_dir)
        os.makedirs(self.work_dir, exist_ok=True)
        requests_cache.install_cache(self.work_dir + '/sportsdb_cache')

    def init_pgs(self):
        def _init_pg(data_dir, url):
            return Postgres(
                port_allocator=self.port_allocator, docker_image=self.pg_docker_image,
                db_data_dir= self.work_dir + '/' + data_dir, url=url
            )

        self.pg = _init_pg('sportsdb_data', self.pg_url)

        if not self.skip_remote_graphql_setup:
            self.remote_pg = _init_pg('remote_sportsdb_data', self.remote_pg_url)

    def init_hges(self):
        def _init_hge(pg, hge_url, log_file):
            return HGE(
                pg=pg, url=hge_url, port_allocator=self.port_allocator,
                args=self.hge_args, log_file= self.work_dir + '/' + log_file,
                docker_image=self.hge_docker_image
            )

        self.hge = _init_hge(self.pg, self.hge_url, 'hge.log')

        if not self.skip_remote_graphql_setup:
            self.remote_hge = _init_hge(self.remote_pg, self.remote_hge_url, 'remote_hge.log')

    @contextmanager
    def graphql_engines_setup(self):
        try:
            self._setup_graphql_engines()
            yield
        finally:
            self.teardown()

    def _setup_graphql_engines(self):

        if not self.hge_docker_image and not self.skip_stack_build:
            HGE.do_stack_build()

        def run_concurrently(threads):
            for thread in threads:
                thread.start()

            for thread in threads:
                thread.join()

        def run_concurrently_fns(*fns):
            threads = [threading.Thread(target=fn) for fn in fns]
            return run_concurrently(threads)

        def set_hge(hge, schema, hge_type):
            pg = hge.pg
            # Schema and data
            pg.run_sql_from_file(sql_file)
            pg.set_id_as_primary_key_for_tables(schema='public')
            pg.move_tables_to_schema('public', schema)

            # Metadata stuff
            hge.track_all_tables_in_schema(schema)
            hge.create_obj_fk_relationships(schema)
            hge.create_arr_fk_relationships(schema)

        def start_remote_postgres_docker():
            if not self.skip_remote_graphql_setup:
                self.remote_pg.start_postgres_docker()

        run_concurrently_fns(
            self.pg.start_postgres_docker,
            start_remote_postgres_docker)

        print("Postgres url:", self.pg.url)

        if not self.skip_remote_graphql_setup:
            print("Remote Postgres url:", self.remote_pg.url)
            self.remote_hge.run()

        self.hge.run()

        # Skip if the tables are already present
        tables = self.pg.get_all_tables_in_a_schema('hge')
        if len(tables) > 0:
            return

        # Download sportsdb
        zip_file = self.download_sportsdb_zip(self.work_dir+ '/sportsdb.zip')
        sql_file = self.unzip_sql_file(zip_file)

        def set_remote_hge():
            if not self.skip_remote_graphql_setup:
                set_hge(self.remote_hge, 'remote_hge', 'Remote')

        # Create the required tables and move them to required schemas
        hge_thread = threading.Thread(
            target=set_hge, args=(self.hge, 'hge', 'Main'))
        remote_hge_thread = threading.Thread(
            target=set_remote_hge)
        run_concurrently([hge_thread, remote_hge_thread])

        if not self.skip_remote_graphql_setup:
            # Add remote_hge as remote schema
            self.hge.add_remote_schema(
                'remote_hge', self.remote_hge.url + '/v1/graphql',
                self.remote_hge.admin_auth_headers()
            )

        # TODO update the remote schema url if needed
        tables = self.pg.get_all_tables_in_a_schema('hdb_catalog')

	    # Create remote relationships only if it is supported
        if 'hdb_remote_relationship' not in tables:
            return

        # Create remote relationships
        if not self.skip_remote_graphql_setup:
            self.create_remote_relationships()

    def create_remote_relationships(self):
        self.hge.create_remote_obj_rel_to_itself('hge', 'remote_hge', 'remote_hge')
        self.hge.create_remote_arr_fk_ish_relationships('hge', 'remote_hge', 'remote_hge')
        self.hge.create_remote_obj_fk_ish_relationships('hge', 'remote_hge', 'remote_hge')

    def teardown(self):
        for res in [self.hge, self.pg]:
            res.teardown()
        if not self.skip_remote_graphql_setup:
            for res in [self.remote_hge, self.remote_pg]:
                res.teardown()

    def unzip_sql_file(self, zip_file):
        with ZipFile(zip_file, 'r') as zip:
            sql_file = zip.infolist()[0]
            print('DB SQL file:', sql_file.filename)
            zip.extract(sql_file, self.work_dir)
        return self.work_dir + '/' + sql_file.filename

    def download_sportsdb_zip(self, filename, url=sportsdb_url):
        with requests.get(url, stream=True) as r:
            r.raise_for_status()
            total = 0
            print()
            with open(filename, 'wb') as f:
                for chunk in r.iter_content(chunk_size=8192):
                    if chunk:
                        total += len(chunk)
                        print("\rDownloaded: ", int(total/1024)  , 'KB', end='')
                        f.write(chunk)
            print('\nDB Zip File:', filename)
        return filename


class HGETestSetupArgs:

    default_pg_docker_image = 'circleci/postgres:11.5-alpine-postgis'

    def __init__(self):
        self.set_arg_parse_options()
        self.parse_args()

    def set_arg_parse_options(self):
        self.arg_parser = argparse.ArgumentParser()
        self.set_pg_options()
        self.set_hge_options()

    def parse_args(self):
        self.parsed_args = self.arg_parser.parse_args()
        self.set_pg_confs()
        self.set_hge_confs()

    def set_pg_confs(self):
        self.pg_url, self.remote_pg_url, self.pg_docker_image = self.get_params(
            ['pg_url', 'remote_pg_url', 'pg_docker_image']
        )
        if not self.pg_url:
            self.pg_docker_image = self.pg_docker_image or self.default_pg_docker_image

    def set_hge_confs(self):
        self.hge_docker_image, self.hge_url, self.remote_hge_url = self.get_params(
            ['hge_docker_image', 'hge_url', 'remote_hge_url']
        )
        self.skip_stack_build = self.parsed_args.skip_stack_build
        self.skip_remote_graphql_setup = self.parsed_args.skip_remote_graphql_setup
        self.hge_args =  self.parsed_args.hge_args[1:]

    def set_pg_options(self):
        pg_opts = self.arg_parser.add_argument_group('Postgres').add_mutually_exclusive_group()
        pg_opts.add_argument('--pg-url', metavar='HASURA_BENCH_PG_URLS', help='Postgres database url to be used for tests', required=False)
        pg_opts.add_argument('--remote-pg-url', metavar='HASURA_BENCH_REMOTE_PG_URLS', help='Url of Postgres database which is attached/has to be attached, with remote graphql-engine', required=False)
        pg_opts.add_argument('--pg-docker-image', metavar='HASURA_BENCH_PG_DOCKER_IMAGE', help='Postgres docker image to be used for tests', required=False)

    def set_hge_options(self):
        hge_opts = self.arg_parser.add_argument_group('Hasura GraphQL Engine')
        hge_opts.add_argument('--hge-url', metavar='HASURA_BENCH_HGE_URL', help='Url of Hasura graphql-engine')
        hge_opts.add_argument('--remote-hge-url', metavar='HASURA_BENCH_REMOTE_HGE_URL', help='Url of remote Hasura graphql-engine')
        hge_opts.add_argument('--hge-docker-image', metavar='HASURA_BENCH_HGE_DOCKER_IMAGE', help='GraphQl engine docker image to be used for tests', required=False)
        hge_opts.add_argument('--skip-stack-build', help='Skip stack build if this option is set', action='store_true', required=False)
        hge_opts.add_argument('--skip-remote-graphql-setup', help='Skip setting up of remote graphql engine', action='store_true', required=False)
        self.arg_parser.add_argument('hge_args', nargs=argparse.REMAINDER)

    def default_env(self, attr):
        return 'HASURA_BENCH_' + attr.upper()

    def get_param(self, attr, env_key=None):
        if not env_key:
            env_key = self.default_env(attr)
        return _first_true([getattr(self.parsed_args, attr), os.getenv(env_key)])

    def get_params(self, params_loc):
        params_out = []
        for param_loc in params_loc:
            # You can specify just the attribute name for the parameter
            if isinstance(param_loc, str):
                attr = param_loc
                env = self.default_env(attr)
            # Or you can specify attribute and environmental variables as a tuple
            else:
                (attr, env) = param_loc
            param = self.get_param(attr, env)
            params_out.append(param)
        return params_out


class HGETestSetupWithArgs(HGETestSetup, HGETestSetupArgs):

    def __init__(self):
        HGETestSetupArgs.__init__(self)
        HGETestSetup.__init__(
            self,
            pg_url = self.pg_url,
            remote_pg_url = self.remote_pg_url,
            pg_docker_image = self.pg_docker_image,
            hge_url = self.hge_url,
            remote_hge_url = self.remote_hge_url,
            hge_docker_image = self.hge_docker_image,
            skip_stack_build = self.skip_stack_build,
            skip_remote_graphql_setup = self.skip_remote_graphql_setup,
            hge_args = self.hge_args
        )

if __name__ == "__main__":
    test_setup = HGETestSetupWithArgs()
    with test_setup.graphql_engines_setup():
        print("Hasura GraphQL engine is running on URL:",test_setup.hge.url+ '/v1/graphql')
        input(Fore.BLUE+'Press Enter to stop GraphQL engine' + Style.RESET_ALL)
