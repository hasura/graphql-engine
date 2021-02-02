import docker
import time
from contextlib import contextmanager
import psycopg2
from psycopg2.sql import SQL, Identifier
from colorama import Fore, Style
import os


class PostgresError(Exception):
    pass


class Postgres:

    def __init__(self, docker_image, db_data_dir, port_allocator, url):
        self.port_allocator =  port_allocator
        self.docker_image = docker_image
        self.db_data_dir = os.path.abspath(db_data_dir)
        self.url = url

    def setup(self):
        if self.docker_image and not self.url:
            self.start_postgres_docker()

    def start_postgres_docker(self):
        if self.url:
            print("Postgres is already running")
            return
        self.port = self.port_allocator.allocate_port(5433)
        self.user = 'hge_test'
        self.password = 'hge_pass'
        self.database = 'hge_test'
        env = {
            'POSTGRES_USER' : self.user,
            'POSTGRES_PASSWORD' : self.password,
            'POSTGRES_DB' : self.database
        }
        docker_ports = {'5432/tcp': ('127.0.0.1', self.port)}
        docker_vols = {
            self.db_data_dir: {
                'bind': '/var/lib/postgresql/data',
                'mode': 'rw'
            }
        }

        self.docker_client = docker.from_env()
        print("Running postgres docker with image:",
              self.docker_image, '(port:{})'.format(self.port))
        cntnr = self.docker_client.containers.run(
            self.docker_image,
            detach=True,
            ports=docker_ports,
            environment=env,
            volumes = docker_vols
        )
        self.pg_container = cntnr
        self.url = 'postgresql://' + self.user + ':' + self.password + '@localhost:' + str(self.port) + '/' + self.database
        print("Waiting for database to be up and running.", end="", flush=True)
        self.wait_for_db_start()
        print("")

    def check_if_container_is_running(self):
        self.pg_container.reload()
        if self.pg_container.status == 'exited':
            raise PostgresError(
                "Postgres docker failed with error: \n" +
                self.pg_container.logs(stdout=True, stderr=True).decode('ascii')
            )

    def wait_for_db_start(self, timeout=60):
        if self.pg_container:
            self.check_if_container_is_running()
        if timeout > 0:
            try:
                self.run_sql('select 1')
                return
            except Exception as e:
                if timeout < 5:
                    print("\nWaiting for database to be up and running:" + repr(e), end=""),
                else:
                    print(".", end="", flush=True),
                sleep_time = 0.5
                time.sleep(sleep_time)
                self.wait_for_db_start(timeout-sleep_time)
        else:
            raise PostgresError("Timeout waiting for database to start")

    @contextmanager
    def cursor(self):
        with psycopg2.connect(self.url) as conn:
            with conn.cursor() as cursor:
                yield cursor

    def get_all_fk_constraints(self, schema='public'):
        with self.cursor() as cursor:
            cursor.execute('''
            SELECT
                tc.table_schema,
                tc.constraint_name,
                tc.table_name,
                kcu.column_name,
                ccu.table_schema AS foreign_table_schema,
                ccu.table_name AS foreign_table_name,
                ccu.column_name AS foreign_column_name
            FROM
                information_schema.table_constraints AS tc
                JOIN information_schema.key_column_usage AS kcu
                    ON tc.constraint_name = kcu.constraint_name
                    AND tc.table_schema = kcu.table_schema
                JOIN information_schema.constraint_column_usage AS ccu
                    ON ccu.constraint_name = tc.constraint_name
                    AND ccu.table_schema = tc.table_schema
            WHERE tc.constraint_type = 'FOREIGN KEY' and tc.table_schema=%s
            ''',(schema,))
            return cursor.fetchall()

    def move_tables_to_schema(self, cur_schema, target_schema):
        print("Moving tables in schema {} to schema {}".format(cur_schema, target_schema))
        table_names = self.get_all_tables_in_a_schema(cur_schema)
        with self.cursor() as cursor:
            cursor.execute(SQL('CREATE SCHEMA IF NOT EXISTS {} ').format(Identifier(target_schema)))
            for table in table_names:
                cursor.execute(SQL('''ALTER TABLE {}.{} SET SCHEMA {};''').format( Identifier(cur_schema), Identifier(table), Identifier(target_schema) ))

    def get_all_columns_of_a_table(self, table_name, table_schema='public'):
        columns = []
        with self.cursor() as cursor:
            cursor.execute('''
            SELECT column_name
            FROM information_schema.columns
            WHERE  table_name= %s and table_schema=%s;
            ''', (table_name, table_schema))
            for row in cursor.fetchall():
                columns.append(row[0])
        return columns

    def get_all_tables_with_column(self, column, schema='public'):
        tables = []
        with self.cursor() as cursor:
            cursor.execute('''
            SELECT table_name
            FROM information_schema.columns
            WHERE  column_name= %s and table_schema=%s;
            ''', (column,schema))
            for row in cursor.fetchall():
                tables.append(row[0])
        return tables

    def set_id_as_primary_key_for_tables(self, schema='public'):
        print("Setting id as primary key for all tables in schema ", schema)
        tables_with_id_col = self.get_all_tables_with_column('id', schema)
        with self.cursor() as cursor:
            for table in tables_with_id_col:
                self.set_id_as_primary_key_for_table(cursor, table, schema)

    def set_id_as_primary_key_for_table(self, cursor, table, schema='public'):
            cursor.execute(SQL('''ALTER TABLE {}.{} ADD PRIMARY KEY (id);''').format( Identifier(schema), Identifier(table) ))

    def get_all_tables_in_a_schema(self, schema='public'):
        tables = []
        with self.cursor() as cursor:
            cursor.execute('''
            SELECT table_name
            FROM information_schema.tables
            WHERE table_schema=%s;
            ''', (schema,))
            for row in cursor.fetchall():
                tables.append(row[0])
        return tables

    def run_sql(self, sql):
        with self.cursor() as cursor:
            cursor.execute(sql)

    def get_server_version(self):
        with self.cursor() as cursor:
            cursor.execute("select setting from pg_config where name = 'VERSION';")
            return cursor.fetchall()[0][0]

    def run_sql_from_file(self, sql_file):
        print("Running sql from file:", sql_file)
        self.run_sql(open(sql_file, 'r').read())

    def teardown(self):
        self.cleanup_docker()

    def cleanup_docker(self):
        if getattr(self, 'pg_container', None):
            cntnr_info = "Postgres docker container " + self.pg_container.name + " " + self.pg_container.image.tags[0]
            print(Fore.YELLOW + "Stopping " + cntnr_info + Style.RESET_ALL)
            self.pg_container.stop()
            print(Fore.YELLOW + "Removing " + cntnr_info + Style.RESET_ALL)
            self.pg_container.remove()
            self.pg_container = None
