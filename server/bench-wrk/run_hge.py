import os
import subprocess
import argparse
import json
import signal
import time
import contextlib
import requests
import inflection
import docker
from colorama import Fore, Style


def rm_file_if_exists(f):
    """Remove a file if it exists"""
    with contextlib.suppress(FileNotFoundError):
        os.remove(f)


class HGEError(Exception):
    """Exception type for class HGE"""


class HGE:

    default_graphql_env = {
        'HASURA_GRAPHQL_ENABLE_TELEMETRY': 'false',
        'EVENT_WEBHOOK_HEADER': "MyEnvValue",
        'HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES': 'true',
        'HASURA_GRAPHQL_CONSOLE_ASSETS_DIR' :  '../../console/static/dist/',
        'HASURA_GRAPHQL_ENABLE_CONSOLE' : 'true'
    }

    def __init__(self, pg, port_allocator, docker_image=None, log_file='hge.log', url=None, args=[]):
        self.pg = pg
        self.log_file = log_file
        if self.log_file:
            self.tix_file = self.log_file[:-4] + '.tix'
        self.docker_image = docker_image
        self.introspection = None
        self.obj_fk_rels = set()
        self.arr_fk_rels = set()
        self.port_allocator = port_allocator
        self.url = url
        self.proc = None
        self.container = None
        self.args = args


    def admin_secret(self):
        admin_secret_env = os.environ.get('HASURA_GRAPHQL_ADMIN_SECRET')
        parser = argparse.ArgumentParser()
        parser.add_argument('--admin-secret', metavar='HASURA_GRAPHQL_ADMIN_SECRET', required=False)
        admin_secret_arg = parser.parse_known_args(self.args)[0].admin_secret
        return admin_secret_arg or admin_secret_env

    @classmethod
    def do_stack_build(cls):
        print(Fore.YELLOW  + "Performing Stack build first" + Style.RESET_ALL)
        # 'stack run' below will also build, but we want to make sure that's a
        # noop so the server starts right away
        subprocess.check_call( ['cabal', 'new-build', 'exe:graphql-engine'])

    def get_hge_env(self):
        hge_env = {
            **os.environ,
            **self.default_graphql_env.copy(),
            'HASURA_GRAPHQL_DATABASE_URL': self.pg.url,
            'HASURA_GRAPHQL_SERVER_PORT': str(self.port),
            'HASURA_GRAPHQL_SERVER_HOST': '127.0.0.1',
            'HPCTIXFILE' : self.tix_file
        }
        return hge_env

    def run(self):
        if self.url:
            return
        if self.docker_image:
            self.run_with_docker()
        else:
            self.run_with_cabal()

    def run_with_docker(self):
        if self.url:
            return
        self.port = self.port_allocator.allocate_port(8080)
        hge_env = self.get_hge_env()
        process_args = ['graphql-engine', 'serve', *self.args]
        docker_ports = {str(self.port) + '/tcp': ('127.0.0.1', self.port)}
        self.docker_client = docker.from_env()
        print("Running GraphQL Engine docker with image:",
              self.docker_image, '(port:{})'.format(self.port))
        print(process_args)
        self.container = self.docker_client.containers.run(
            self.docker_image,
            command=process_args,
            detach=True,
            ports=docker_ports,
            environment=hge_env,
            network_mode='host',
            volumes={}
        )
        self.url = 'http://127.0.0.1:' + str(self.port)
        print("Waiting for GraphQL Engine to be running.", end='')
        self.wait_for_start()


    def run_with_cabal(self):
        if self.url:
            return
        self.port = self.port_allocator.allocate_port(8080)
        rm_file_if_exists(self.tix_file)
        hge_env = self.get_hge_env()
        process_args = ['cabal', 'new-run', '--', 'exe:graphql-engine', 'serve', *self.args]
        print("Running GraphQL with 'cabal run': (port:{})".format(self.port))
        print(process_args)
        self.log_fp = open(self.log_file, 'w')
        self.proc = subprocess.Popen(
            process_args,
            env=hge_env,
            shell=False,
            bufsize=-1,
            start_new_session=True,
            stdout=self.log_fp,
            stderr=subprocess.STDOUT
        )
        self.url = 'http://127.0.0.1:' + str(self.port)
        print("Waiting for GraphQL Engine to be running.", end='')
        self.wait_for_start()

    def check_if_process_is_running(self):
        if self.proc.poll() is not None:
            with open(self.log_file) as fr:
                raise HGEError(
                        "GraphQL engine failed with error: " + fr.read())

    def check_if_container_is_running(self):
        self.container.reload()
        if self.container.status == 'exited':
            raise HGEError(
                "GraphQL engine failed with error: \n" +
                self.container.logs(stdout=True, stderr=True).decode('ascii')
            )

    def wait_for_start(self, timeout=120):
        if timeout <= 0:
            raise HGEError("Timeout waiting for graphql process to start")
        if self.proc:
            self.check_if_process_is_running()
        elif self.container:
            self.check_if_container_is_running()
        try:
            q = { 'query': 'query { __typename }' }
            r = requests.post(self.url + '/v1/graphql',json.dumps(q),headers=self.admin_auth_headers())
            if r.status_code == 200:
                print()
                return
        except requests.exceptions.ConnectionError:
            pass
        except ConnectionError:
            pass
        print(".", end="", flush=True),
        sleep_time = 0.5
        time.sleep(sleep_time)
        self.wait_for_start(timeout - sleep_time)

    def teardown(self):
        if getattr(self, 'log_fp', None):
            self.log_fp.close()
            self.log_fp = None
        if self.proc:
            self.cleanup_process()
        elif self.container:
            self.cleanup_docker()

    def cleanup_process(self):
        # TODO hangs
            print(Fore.YELLOW + "Stopping graphql engine at port:", self.port, Style.RESET_ALL)

            pgrp = os.getpgid(self.proc.pid)
            os.killpg(pgrp, signal.SIGTERM) 
            # NOTE this doesn't seem to work, although a SIGINT from terminal does ...
            # self.proc.send_signal(signal.SIGINT)
            self.proc.wait()
            self.proc = None

    def cleanup_docker(self):
        cntnr_info = "HGE docker container " + self.container.name + " " + repr(self.container.image)
        print(Fore.YELLOW + "Stopping " + cntnr_info + Style.RESET_ALL)
        self.container.stop()
        print(Fore.YELLOW + "Removing " + cntnr_info + Style.RESET_ALL)
        self.container.remove()
        self.container = None

    def admin_auth_headers(self):
        headers = {}
        if self.admin_secret():
            headers['X-Hasura-Admin-Secret'] = self.admin_secret()
        return headers

    def v1q(self, q, exp_status=200):
        resp = requests.post(self.url + '/v1/query', json.dumps(q), headers=self.admin_auth_headers())
        assert resp.status_code == exp_status, (resp.status_code, resp.json())
        return resp.json()

    def graphql_q(self, query, variables={}, exp_status = 200):
        q = {'query': query}
        if variables:
            q['variables'] = variables
        resp = requests.post(self.url + '/v1/graphql', json.dumps(q), headers=self.admin_auth_headers())
        assert resp.status_code == exp_status, (resp.status_code, resp.json())
        assert 'errors' not in resp.json(), resp.json()
        return resp.json()

    def track_all_tables_in_schema(self, schema='public'):
        print("Track all tables in schema ", schema)
        all_tables = self.pg.get_all_tables_in_a_schema(schema)
        all_tables = [ {'schema': schema, 'name': t}
                       for t in all_tables ]
        return self.track_tables(all_tables)

    def run_bulk(self, queries, exp_status = 200):
        bulk_q = {
            'type': 'bulk',
            'args': queries
        }
        return self.v1q(bulk_q, exp_status)

    def select_simple(self, table, columns):
        query = {
            'type': 'select',
            'args': {
                'table': table,
                'columns': columns
            }
        }
        return self.v1q(query)

    def get_all_tracked_tables(self):
        table = {
            'schema': 'hdb_catalog',
            'name': 'hdb_table'
        }
        columns = ['table_schema', 'table_name']
        resp = self.select_simple(table, columns)
        tables = []
        for row in resp:
            tables.append({
                'schema': row['table_schema'],
                'name': row['table_name']
            })
        return tables


    def track_tables(self, tables, exp_status=200):
        queries = []
        for table in tables:
            q = {
                'type' : 'track_table',
                'args' : table
            }
            queries.append(q)
        return self.run_bulk(queries, exp_status)

    def track_table(self, table, exp_status=200):
        q = self.mk_track_table_q(table)
        return self.v1q(q, exp_status)

    def mk_track_table_q(self, table):
        return {
            'type' : 'track_table',
            'args' : table
        }

    def add_remote_schema(self, name, remote_url, headers={}, client_hdrs=False):
        def hdr_name_val_pair(headers):
            nvp = []
            for (k,v) in headers.items():
                nvp.append({'name': k, 'value': v})
            return nvp
        if len(headers) > 0:
            client_hdrs = True
        q = {
            'type' : 'add_remote_schema',
            'args': {
                'name': name,
                'comment': name,
                'definition': {
                    'url': remote_url,
                    'headers':  hdr_name_val_pair(headers),
                    'forward_client_headers': client_hdrs
                }
            }
        }
        return self.v1q(q)


    def create_remote_obj_rel_to_itself(self, tables_schema, remote, remote_tables_schema):
        print("Creating remote relationship to the tables in schema {} to itself using remote {}".format(tables_schema, remote))
        fk_constrnts = self.pg.get_all_fk_constraints(tables_schema)
        for (s, _, t, c, _, ft, _) in fk_constrnts:
            table_cols = self.pg.get_all_columns_of_a_table(t, s)
            if not 'id' in table_cols:
                continue
            rel_name = 'remote_' + inflection.singularize(t) + '_via_' + c
            query ={
                'type': 'create_remote_relationship',
                'args' : {
                    'name' : rel_name,
                    'table' : {
                        'schema': s,
                        'name': t
                    },
                    'remote_schema': remote,
                    'hasura_fields': ['id', c],
                    'remote_field': {
                        remote_tables_schema + '_' + ft + '_by_pk' : {
                            'arguments': {
                                'id':  '$' + c
                            },
                            'field': {
                                inflection.pluralize(t) + '_by_' + c: {
                                    'arguments' : {
                                        'where': {
                                            c : {
                                                '_eq': '$id'
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            print(query)
            self.v1q(query)

    def create_remote_obj_fk_ish_relationships(self, tables_schema, remote, remote_tables_schema):
        print("Creating object foreign key ish relationships for tables in schema {} using remote {}".format(tables_schema, remote))
        fk_constrnts = self.pg.get_all_fk_constraints(tables_schema)
        for (s, _, t, c, _, ft, _) in fk_constrnts:
            rel_name = inflection.singularize(ft)
            if c.endswith('_id'):
                rel_name = c[:-3]
            rel_name = 'remote_' + rel_name
            query ={
                'type': 'create_remote_relationship',
                'args' : {
                    'name' : rel_name,
                    'table' : {
                        'schema': s,
                        'name': t
                    },
                    'remote_schema': remote,
                    'hasura_fields': [c],
                    'remote_field': {
                        remote_tables_schema + '_' + ft + '_by_pk' : {
                            'arguments' : {
                                'id':  '$' + c
                            }
                        }
                    }

                }
            }
            print(query)
            self.v1q(query)

    def create_obj_fk_relationships(self, schema='public'):
        print("Creating object foreign key relationships for tables in schema ", schema)
        fk_constrnts = self.pg.get_all_fk_constraints(schema)
        queries = []
        for (s, _, t, c, _, ft, _) in fk_constrnts:
            rel_name = inflection.singularize(ft)
            if c.endswith('_id'):
                rel_name = c[:-3]
            table_cols = self.pg.get_all_columns_of_a_table(t, s)
            if rel_name in table_cols:
                rel_name += '_' + inflection.singularize(ft)
            queries.append({
                'type' : 'create_object_relationship',
                'args': {
                    'table': {
                        'schema': s,
                        'name': t
                    },
                    'name': rel_name,
                    'using': {
                        'foreign_key_constraint_on': c
                    }
                }
            })
            self.obj_fk_rels.add(((s,t),rel_name))
        return self.run_bulk(queries)

    def create_remote_arr_fk_ish_relationships(self, tables_schema, remote, remote_tables_schema):
        fk_constrnts = self.pg.get_all_fk_constraints(tables_schema)
        for (_, _, t, c, fs, ft, _) in fk_constrnts:
            rel_name = 'remote_' + inflection.pluralize(t) + '_by_' + c
            query ={
                'type': 'create_remote_relationship',
                'args' : {
                    'name' : rel_name,
                    'table' : {
                        'schema': fs,
                        'name': ft
                    },
                    'remote_schema': remote,
                    'hasura_fields': ['id'],
                    'remote_field': {
                        remote_tables_schema + '_' + t : {
                            'arguments' : {
                                'where': {
                                    c : {
                                        '_eq': '$id'
                                    }
                                }
                            }
                        }
                    }
                }
            }
            print(query)
            self.v1q(query)

    def create_arr_fk_relationships(self, schema='public'):
        print("Creating array foreign key relationships for tables in schema ", schema)
        fk_constrnts = self.pg.get_all_fk_constraints(schema)
        queries = []
        for (s, _, t, c, fs, ft, _) in fk_constrnts:
            rel_name = inflection.pluralize(t) + '_by_' + c
            queries.append({
                'type' : 'create_array_relationship',
                'args': {
                    'table': {
                        'schema': fs,
                        'name': ft
                    },
                    'name': rel_name,
                    'using': {
                        'foreign_key_constraint_on': {
                            'table': {
                                'schema': s,
                                'name': t
                            },
                            'column': c
                        }
                    }
                }
            })
            self.arr_fk_rels.add(((fs,ft),rel_name))
        return self.run_bulk(queries)

    def run_sql(self, sql):
        """Run given SQL query"""
        def mk_run_sql_q(sql):
            return {
                'type' : 'run_sql',
                'args': {
                    'sql' : sql
                }
            }
        return self.v1q(mk_run_sql_q(sql))
