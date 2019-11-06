from sportsdb_setup import HGETestSetup, HGETestSetupArgs
from run_hge import HGE
import graphql
import multiprocessing
import json
import os
import docker
import ruamel.yaml as yaml
import cpuinfo
import subprocess
import threading
import time
from colorama import Fore, Style
from plot import run_dash_server
import webbrowser


fileLoc = os.path.dirname(os.path.abspath(__file__))


class HGEWrkBench(HGETestSetup):

    wrk_docker_image = 'hasura/wrk:v0.1'

    lua_dir = '/root/bench_scripts'

    def __init__(
            self, pg_urls, pg_docker_image, hge_docker_image=None,
            hge_admin_secret=None, skip_stack_build=False,
            graphql_queries_file='queries.graphql', connections=50,
            duration=300, results_hge_url = None, results_hge_admin_secret = None
    ):
        self.load_queries(graphql_queries_file)
        super().__init__(pg_urls, pg_docker_image, hge_docker_image, hge_admin_secret, skip_stack_build)
        self.connections = connections
        self.duration = duration
        self.results_hge_url = results_hge_url
        self.results_hge_admin_secret = results_hge_admin_secret
        self.extract_cpu_info()

    def load_queries(self, graphql_queries_file):
        self.graphql_queries_file = graphql_queries_file
        with open(self.graphql_queries_file) as f:
            queries = f.read()
        self.queryNames = []
        self.queries = []
        for oper in graphql.parse(queries).definitions:
            self.queryNames.append(oper.name.value)
            self.queries.append(oper)

    def get_wrk_params(self):
        cpu_count = multiprocessing.cpu_count()
        return {
            'threads': cpu_count,
            'connections': self.connections,
            'duration': self.duration
        }

    def run_test(self, query):
        query_str = graphql.print_ast(query)
        print(Fore.GREEN + "Running benchmark for query\n", query_str + Style.RESET_ALL)
        self.hge.graphql_q(query_str) # Test query once for errors
        bench_script = os.path.join(self.lua_dir + '/bench.lua')
        graphql_url = self.hge.url + '/v1/graphql'
        lua_env = {
            'LUA_PATH': '/usr/share/lua/5.1/?.lua;' +
            os.path.join(self.lua_dir,  '?.lua') + ';;'
        }
        scripts_vol = {
            os.path.join(fileLoc, 'bench_scripts'): {
                'bind' : self.lua_dir,
                'mode' : 'ro'
            }
        }
        params = self.get_wrk_params()
        wrk_command = [
            'wrk',
            '-t', str(params['threads']),
            '-c', str(params['connections']),
            '-d', str(params['duration']),
            '--latency',
            '-s', bench_script,
            graphql_url,
            query_str
        ]
        self.docker_client = docker.from_env()
        result = self.docker_client.containers.run(
            self.wrk_docker_image,
            detach = False,
            stdout = False,
            stderr = True,
            command = wrk_command,
            network_mode = 'host',
            environment = lua_env,
            volumes = scripts_vol,
            remove = True
        )
        result = json.loads(result)
        print(json.dumps(result, indent=2))
        self.insert_result(query, result)

    def get_version(self):
        script = os.path.join(fileLoc, 'gen-version.sh')
        return subprocess.check_output([script]).decode('ascii').strip()

    def get_server_shasum(self):
        script = os.path.join(fileLoc, 'get-server-sha.sh')
        return subprocess.check_output([script]).decode('ascii').strip()

    def extract_cpu_info(self):
        self.cpu_info = cpuinfo.get_cpu_info()
        for k in ['flags', 'python_version', 'hz_actual', 'hz_actual_raw']:
            if self.cpu_info.get(k):
                del self.cpu_info[k]

    def get_results(self):
        query = '''
query results {
  hge_bench_results_avg {
    query_name
    requests_per_sec
    docker_image
    version
  }
}
        '''
        output = self.results_hge.graphql_q(query)
        return output['data']['hge_bench_results_avg']

    def plot_results(self):
        def open_plot_in_browser():
            time.sleep(1)
            webbrowser.open_new_tab('http://127.0.0.1:8050/')
        threading.Thread(target=open_plot_in_browser).start()
        run_dash_server(self.get_results())

    def gen_result_insert_var(self, query, result):
        insert_var = dict()
        def set_cpu_info():
            cpu_key = self.cpu_info['brand'] + ' vCPUs: ' + str(self.cpu_info['count'])
            print("CPU KEY:", cpu_key)
            insert_var['cpu']= {
                'data' : {
                    'info': self.cpu_info,
                    'key': cpu_key
                },
                "on_conflict": {
                    "constraint": "cpu_info_pkey",
                    "update_columns": "key"
                }
            }

        def set_query_info():
            insert_var["query"] = {
                "data": {
                    "name" : query.name.value,
                    "query" : graphql.print_ast(query)
                },
                "on_conflict" : {
                    "constraint": "gql_query_query_key",
                    "update_columns": "query"
                }
            }

        def set_version_info():
            if self.hge_docker_image:
                insert_var["docker_image"] = self.hge_docker_image
            else:
                insert_var["version"] = self.get_version()
                insert_var["server_shasum"] = self.get_server_shasum()
            insert_var['postgres_version'] = self.pg.get_server_version()

        def set_wrk_params():
            insert_var['wrk_parameters'] = self.get_wrk_params()

        insert_var["latency"] = result['latency']
        insert_var["requests_per_sec"] = result['requests']
        set_cpu_info()
        set_query_info()
        set_version_info()
        set_wrk_params()
        return insert_var

    def insert_result(self, query, result):
        result_var = self.gen_result_insert_var(query, result)
        insert_query = """
mutation insertResult($result: hge_bench_results_insert_input!) {
  insert_hge_bench_results(objects: [$result]){
    affected_rows
  }
}"""
        variables = {'result': result_var}
        self.results_hge.graphql_q(insert_query, variables)

    def setup_results_schema(self):
        if not self.results_hge_url:
            self.results_hge_url = self.hge.url
            self.results_hge_admin_secret = self.hge.admin_secret
        self.results_hge = HGE(None, None, admin_secret=self.results_hge_admin_secret, log_file=None, url=self.results_hge_url)
        results_table = {
            'name' : 'results',
            'schema': 'hge_bench'
        }
        if results_table in self.results_hge.get_all_tracked_tables():
            return
        schema_file = os.path.join(fileLoc, 'results_schema.yaml')
        with open(schema_file) as f:
            queries = yaml.safe_load(f)
        self.results_hge.run_bulk(queries)

    def run_github_hook(self):
        pass

    def run_tests(self):
        with self.graphql_engines_setup():
            self.setup_results_schema()
            for query in self.queries:
                try:
                    self.run_test(query)
                except Exception:
                    print(Fore.RED + "Benchmarking graphql query '" + query.name.value + "' failed" + Style.RESET_ALL)
                    raise
            if not self.skip_plots:
                self.plot_results()


class HGEWrkBenchArgs(HGETestSetupArgs):

    def __init__(self):
        self.set_arg_parse_options()
        self.parse_args()

    def set_arg_parse_options(self):
        HGETestSetupArgs.set_arg_parse_options(self)
        self.set_wrk_options()

    def parse_args(self):
        HGETestSetupArgs.parse_args(self)
        self.parse_wrk_options()

    def set_wrk_options(self):
        wrk_opts = self.arg_parser.add_argument_group('wrk')
        wrk_opts.add_argument('--queries-file', metavar='HASURA_BENCH_QUERIES_FILE', help='Queries file for benchmarks', default='queries.graphql')
        wrk_opts.add_argument('--connections', metavar='HASURA_BENCH_CONNECTIONS', help='Total number of open connections', default=50)
        wrk_opts.add_argument('--duration', metavar='HASURA_BENCH_DURATION', help='Duration of tests in seconds', default=300)
        wrk_opts.add_argument('--results-hge-url', metavar='HASURA_BENCH_RESULTS_HGE_URL', help='The GraphQL engine to which the results should be uploaded', required=False)
        wrk_opts.add_argument('--results-hge-admin-secret', metavar='HASURA_BENCH_RESULTS_HGE_ADMIN_SECRET', help='Admin secret of the GraphQL engine to which the results should be uploaded', required=False)
        wrk_opts.add_argument('--skip-plots', help='Skip plotting', action='store_true', required=False)

    def parse_wrk_options(self):
        self.connections, self.duration, self.graphql_queries_file, self.res_hge_url, self.res_hge_admin_secret = \
            self.get_params([
                ('connections', 'HASURA_BENCH_CONNECTIONS'),
                ('duration', 'HASURA_BENCH_DURATION'),
                ('queries_file', 'HASURA_BENCH_QUERIES_FILE'),
                ('results_hge_url', 'HASURA_BENCH_RESULTS_HGE_URL'),
                ('results_hge_admin_secret', 'HASURA_BENCH_RESULTS_HGE_ADMIN_SECRET')
            ])
        self.skip_plots = self.parsed_args.skip_plots


class HGEWrkBenchWithArgs(HGEWrkBenchArgs, HGEWrkBench):

    def __init__(self):
        HGEWrkBenchArgs.__init__(self)
        HGEWrkBench.__init__(
            self,
            pg_urls = self.pg_urls,
            pg_docker_image = self.pg_docker_image,
            hge_docker_image = self.hge_docker_image,
            hge_admin_secret = self.hge_admin_secret,
            skip_stack_build = self.skip_stack_build,
            graphql_queries_file = self.graphql_queries_file,
            connections = self.connections,
            duration = self.duration
        )
        # TODO initialize values

    # TODO Any extra argument passed after -- should go to GraphQL Engine
    # Allow environmental variables to be passed to GraphQL Engine even if run as a docker image
    #TODO Store results in database
    #TODO look at argparse groups and exclusive argument groups
    #TODO Ensure that no errors occur

    # Test parameters
    # Machine type, query, docker image or commit i
    # Results
    # queries per second, latency, latency standard deviation and percentiles

if __name__ == "__main__":
    bench = HGEWrkBenchWithArgs()
    bench.run_tests()
