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
import datetime
from colorama import Fore, Style
from plot import run_dash_server
import webbrowser
import pathlib
from urllib.parse import urlparse, urlunparse
import boto3


fileLoc = os.path.dirname(os.path.abspath(__file__))

def uri_path_join(uri, *paths):
    p = urlparse(uri)
    new_path = os.path.join(p.path, *paths)
    return urlunparse(p._replace(path=new_path))


class HGEWrkBench(HGETestSetup):

    wrk_docker_image = 'hasura/wrk:v0.3'

    # We'll bind mount the lua script dir to this directory within the wrk container:
    lua_dir = '/tmp/bench_scripts'

    rps_steps = [10, 20, 50, 100, 200, 500, 1000, 2000, 5000]

    def __init__(
            self, pg_url, remote_pg_url, pg_docker_image, hge_url=None,
            remote_hge_url=None, hge_docker_image=None,
            hge_args=[], skip_stack_build=False,
            graphql_queries_file='queries.graphql', connections=50,
            duration=300, results_hge_url = None, results_hge_admin_secret = None
    ):
        self.load_queries(graphql_queries_file)
        super().__init__(
            pg_url = pg_url,
            remote_pg_url = remote_pg_url,
            pg_docker_image = pg_docker_image,
            hge_url = hge_url,
            remote_hge_url = remote_hge_url,
            hge_docker_image = hge_docker_image,
            hge_args = hge_args,
            skip_stack_build = skip_stack_build
        )
        self.connections = connections
        self.duration = duration
        self.results_hge_url = results_hge_url
        self.results_hge_admin_secret = results_hge_admin_secret
        self.extract_cpu_info()

    def load_queries(self, graphql_queries_file):
        self.graphql_queries_file = graphql_queries_file
        with open(self.graphql_queries_file) as f:
            queries = f.read()
        self.query_names = []
        self.queries = []
        for oper in graphql.parse(queries).definitions:
            self.query_names.append(oper.name.value)
            self.queries.append(oper)

    def get_wrk2_params(self):
        cpu_count = multiprocessing.cpu_count()
        return {
            'threads': cpu_count,
            'connections': self.connections,
            'duration': self.duration
        }

    def get_current_user(self):
        return '{}:{}'.format(os.geteuid(), os.getegid())

    def wrk2_test(self, query, rps):
        def upload_files(files):
            if self.upload_root_uri:
                p = urlparse(self.upload_root_uri)
                if p.scheme == 's3':
                    bucket = p.netloc
                    key = p.path.lstrip('/')
                    s3_client = boto3.client('s3')
                    for (f, f_key) in files:
                        s3_client.upload_file(f, bucket, os.path.join(key, f_key))

        query_str = graphql.print_ast(query)
        params = self.get_wrk2_params()
        print(Fore.GREEN + "Running benchmark wrk2 for at {} req/s (duration: {}) for query\n".format(rps, params['duration']), query_str +  Style.RESET_ALL)
        bench_script = os.path.join(self.lua_dir, 'bench-wrk2.lua')
        graphql_url = self.hge.url + '/v1/graphql'
        timestamp = datetime.datetime.now().strftime('%Y-%m-%d_%H-%M-%S')
        results_dir = self.results_root_dir
        tests_path = [str(rps), timestamp]
        results_dir = os.path.join(results_dir, *tests_path)
        os.makedirs(results_dir, exist_ok=True)
        wrk2_command = [
            'wrk2',
            '-R', str(rps),
            '-t', str(params['threads']),
            '-c', str(params['connections']),
            '-d', str(params['duration']),
            '--latency',
            '-s', bench_script,
            graphql_url,
            query_str,
            results_dir
        ]
        volumes = self.get_scripts_vol()
        volumes[results_dir] = {
            'bind': results_dir,
            'mode': 'rw'
        }
        self.docker_client = docker.from_env()
        result = self.docker_client.containers.run(
            self.wrk_docker_image,
            detach = False,
            stdout = True,
            stderr = False,
            command = wrk2_command,
            network_mode = 'host',
            environment = self.get_lua_env(),
            volumes = volumes,
            remove = True,
            user = self.get_current_user()
        ).decode('ascii')
        histogram_file = os.path.join(results_dir, 'latencies.hgrm')
        histogram = self.get_latency_histogram(result, histogram_file)

        summary_file = os.path.join(results_dir, 'summary.json')
        with open(summary_file) as f:
            summary = json.load(f)
        latencies_file = os.path.join(results_dir, 'latencies')

        def extract_data(v):
            return v['data'] if isinstance(v, dict) and 'data' in v else v
        tests_info = { k:extract_data(v) for (k, v) in self.gen_test_info(query, rps).items() }
        tests_setup_file = os.path.join(results_dir, 'test_setup.json')
        with open(tests_setup_file, 'w') as f:
            f.write(json.dumps(tests_info, indent=2))

        upload_files([
            (x, os.path.join(*tests_path,y))
            for (x,y) in [
                    (summary_file, 'summary.json'),
                    (latencies_file, 'latencies'),
                    (histogram_file, 'latencies.hgrm'),
                    (tests_setup_file, 'test_setup.json')
            ]
        ])
        if self.upload_root_uri:
            latencies_uri = uri_path_join(self.upload_root_uri, *tests_path, 'latencies')
        else:
            latencies_uri = pathlib.Path(latencies_file).as_uri()
        self.insert_result(query, rps, summary, histogram, latencies_uri)
        return (summary, histogram)

    def get_latency_histogram(self, result, write_histogram_file):
        const_true = lambda l : True
        state_changes = {
            'start' : {
                (lambda l: 'Detailed Percentile spectrum' in l) :  'histogram_start'
            },
            'histogram_start': {
                (lambda l: 'Value' in l and 'Percentile' in l): 'histogram_headers'
            },
            'histogram_headers': {
                const_true: 'histogram_empty_line'
            },
            'histogram_empty_line' : {
                const_true: 'histogram_values'
            },
            'histogram_values': {
                (lambda l: l.strip().startswith('#')): 'histogram_summary'
            },
            'histogram_summary': {
                (lambda l: not l.strip().startswith('#')): 'histogram_end'
            }
        }
        state = 'start'
        histogram = []
        print(Fore.CYAN + "Latency histogram summary" + Style.RESET_ALL)
        with open(write_histogram_file, 'w') as f:
            for line in result.splitlines():
                # Change the state
                for (check, next_state) in state_changes[state].items():
                    if check(line):
                        state = next_state
                        break
                if state == 'start':
                    continue
                elif state == 'histogram_end':
                    break
                if state == 'histogram_summary':
                    print(Fore.CYAN + line + Style.RESET_ALL)
                if state in ['histogram_headers','histogram_values','histogram_summary']:
                    f.write(line+'\n')

                if state == 'histogram_values':
                    (val, percentile, total_count, _) = line.strip().split()
                    histogram.append({
                        'percentile': float(percentile),
                        'latency': float(val),
                        'total_count': float(total_count)
                    })
        return histogram

    # The appropriate Lua env vars for execution within wrk container:
    def get_lua_env(self):
        return {
            'LUA_PATH': '/usr/share/lua/5.1/?.lua;' +
            os.path.join(self.lua_dir,  '?.lua') + ';;',
            'LUA_CPATH': '/usr/lib/lua/5.1/?.so;/usr/lib/x86_64-linux-gnu/lua/5.1/?.so;;'
        }

    def get_scripts_vol(self):
        return {
            os.path.join(fileLoc, 'wrk-websocket-server', 'bench_scripts'): {
                'bind' : self.lua_dir,
                'mode' : 'ro'
            }
        }

    def max_rps_test(self, query):
        query_str = graphql.print_ast(query)
        print(Fore.GREEN + "(Compute maximum Request per second) Running wrk benchmark for query\n", query_str + Style.RESET_ALL)
        self.hge.graphql_q(query_str) # Test query once for errors
        bench_script = os.path.join(self.lua_dir + '/bench-wrk.lua')
        graphql_url = self.hge.url + '/v1/graphql'
        params = self.get_wrk2_params()
        duration = 30
        wrk_command = [
            'wrk',
            '-t', str(params['threads']),
            '-c', str(params['connections']),
            '-d', str(duration),
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
            environment = self.get_lua_env(),
            volumes = self.get_scripts_vol(),
            remove = True,
            user = self.get_current_user()
        )
        summary = json.loads(result)['summary']
        # TODO explain this calculation. Why aren't we using wrk's reported 'max'? Should we call this avg_sustained_rps or something?
        max_rps = round(summary['requests']/float(duration))
        self.insert_max_rps_result(query, max_rps)
        print("Max RPS", max_rps)
        return max_rps

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
  latency: hge_bench_latest_results {
    query_name
    requests_per_sec
    docker_image
    version
    latencies_uri
    latency_histogram {
      percentile
      latency
    }
  }
  max_rps: hge_bench_avg_query_max_rps {
    query_name
    docker_image
    version
    max_rps
  }
}
        '''
        output = self.results_hge.graphql_q(query)
        return output['data']

    def set_cpu_info(self, insert_var):
        cpu_key = self.cpu_info['brand'] + ' vCPUs: ' + str(self.cpu_info['count'])
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


    def set_query_info(self, insert_var, query):
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

    #TODO add executable shasum also
    def set_version_info(self, insert_var):
        if self.hge_docker_image:
            insert_var["docker_image"] = self.hge_docker_image
        else:
            insert_var["version"] = self.get_version()
            insert_var["server_shasum"] = self.get_server_shasum()

        insert_var['postgres_version'] = self.pg.get_server_version()
        if self.scenario_name:
            insert_var['scenario_name'] = self.scenario_name

    def set_hge_args_env_vars(self, insert_var):
        to_hide_env = ['HASURA_GRAPHQL_' + env for env in
                       [ 'ADMIN_SECRET', 'DATABASE_URL', 'JWT_SECRET']
        ]
        env = { k:v for (k,v) in self.hge.get_hge_env().items() if (k.startswith('HASURA_GRAPHQL') and k not in to_hide_env) or k in ['GHCRTS'] }
        args = self.hge.args
        insert_var['hge_conf']  = {
            'env': env,
            'args': args
        }

    def gen_max_rps_insert_var(self, query, max_rps):
        insert_var = dict()
        self.set_cpu_info(insert_var)
        self.set_query_info(insert_var, query)
        self.set_version_info(insert_var)
        self.set_hge_args_env_vars(insert_var)
        insert_var['max_rps'] = max_rps
        insert_var['wrk_parameters'] = self.get_wrk2_params()
        return insert_var

    def plot_results(self):
        def open_plot_in_browser():
            time.sleep(1)
            webbrowser.open_new_tab('http://127.0.0.1:8050/')
        threading.Thread(target=open_plot_in_browser).start()
        run_dash_server(self.get_results())

    # Collect info about the test environment
    def gen_test_info(self, query, rps):
        test_info = dict()
        self.set_cpu_info(test_info)
        self.set_query_info(test_info, query)
        self.set_version_info(test_info)
        self.set_hge_args_env_vars(test_info)
        test_info["requests_per_sec"] = rps
        test_info['wrk2_parameters'] = self.get_wrk2_params()
        return test_info

    def gen_result_insert_var(self, query, rps, summary, latency_histogram, latencies_uri):
        insert_var = self.gen_test_info(query, rps)
        insert_var["summary"] = summary
        insert_var['latency_histogram'] = {
            'data' : latency_histogram
        }
        insert_var['latencies_uri'] = latencies_uri
        return insert_var

    def insert_result(self, query, rps, summary, latency_histogram, latencies_uri):
        result_var = self.gen_result_insert_var(query, rps, summary, latency_histogram, latencies_uri)
        insert_query = """
mutation insertResult($result: hge_bench_results_insert_input!) {
  insert_hge_bench_results(objects: [$result]){
    affected_rows
  }
}"""
        variables = {'result': result_var}
        self.results_hge.graphql_q(insert_query, variables)

    def insert_max_rps_result(self, query, max_rps):
        result_var = self.gen_max_rps_insert_var(query, max_rps)
        insert_query = """
mutation insertMaxRps($result: hge_bench_query_max_rps_insert_input!) {
  insert_hge_bench_query_max_rps(objects: [$result]){
    affected_rows
  }
}"""
        variables = {'result': result_var}
        self.results_hge.graphql_q(insert_query, variables)

    def setup_results_schema(self):
        if not self.results_hge_url:
            self.results_hge_url = self.hge.url
            self.results_hge_admin_secret = self.hge.admin_secret()
        if self.results_hge_admin_secret:
            results_hge_args = ['--admin-secret', self.results_hge_admin_secret]
        else:
            results_hge_args = []
        self.results_hge = HGE(None, None, args=results_hge_args, log_file=None, url=self.results_hge_url)
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


    def run_query_benchmarks(self):
        def get_results_root_dir(query):
            if self.hge_docker_image:
                ver_info = 'docker-tag-' + self.hge_docker_image.split(':')[1]
            else:
                ver_info = self.get_version()
            query_name = query.name.value
            results_root_dir = os.path.abspath(self.work_dir)
            return os.path.join(results_root_dir, ver_info, query_name)

        for query in self.queries:
            try:
                self.results_root_dir = get_results_root_dir(query)
                max_rps = self.max_rps_test(query)
                # The tests should definitely not be running very close to or higher than maximum requests per second
                rps_steps = [ r for r in self.rps_steps if r < 0.6*max_rps]
                print("Benchmarking queries with wrk2 for the following requests/sec", rps_steps)
                for rps in rps_steps:
                    if rps < int(0.6*max_rps):
                        self.wrk2_test(query, rps)
            except Exception:
                print(Fore.RED + "Benchmarking Graphql Query '" + query.name.value + "' failed" + Style.RESET_ALL)
                raise

    def run_tests(self):
        with self.graphql_engines_setup():
            self.setup_results_schema()
            if self.run_benchmarks:
                self.run_query_benchmarks()
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
        def boolean_string(s):
            s = s.lower()
            if s not in {'false', 'true'}:
                raise ValueError('Not a valid boolean string')
            return s == 'true'
        wrk_opts = self.arg_parser.add_argument_group('wrk')
        wrk_opts.add_argument('--queries-file', metavar='HASURA_BENCH_QUERIES_FILE', help='Queries file for benchmarks', default='queries.graphql')
        wrk_opts.add_argument('--connections', metavar='HASURA_BENCH_CONNECTIONS', help='Total number of open connections', default=50)
        wrk_opts.add_argument('--duration', metavar='HASURA_BENCH_DURATION', help='Duration of tests in seconds', default=300)
        wrk_opts.add_argument('--upload-root-uri', metavar='HASURA_BENCH_UPLOAD_ROOT_URI', help='The URI to which the latency results should be uploaded. Curently only s3 is supported', required=False)
        wrk_opts.add_argument('--set-scenario-name', metavar='HASURA_BENCH_SCENARIO_NAME', help='Set a name for the test scenario. This will be shown in logs', required=False)
        wrk_opts.add_argument('--results-hge-url', metavar='HASURA_BENCH_RESULTS_HGE_URL', help='The GraphQL engine to which the results should be uploaded', required=False)
        wrk_opts.add_argument('--results-hge-admin-secret', metavar='HASURA_BENCH_RESULTS_HGE_ADMIN_SECRET', help='Admin secret of the GraphQL engine to which the results should be uploaded', required=False)
        wrk_opts.add_argument('--skip-plots', help='Skip plotting', action='store_true', required=False)
        wrk_opts.add_argument('--run-benchmarks', metavar='HASURA_BENCH_RUN_BENCHMARKS', help='Whether benchmarks should be run or not', default=True, type=boolean_string)

    def get_s3_caller_identity(self):
        return boto3.client('sts').get_caller_identity()

    def parse_wrk_options(self):
        self.connections, self.duration, self.graphql_queries_file, self.res_hge_url, upload_root_uri, self.res_hge_admin_secret, self.run_benchmarks, self.scenario_name = \
            self.get_params([
                ('connections', 'HASURA_BENCH_CONNECTIONS'),
                ('duration', 'HASURA_BENCH_DURATION'),
                ('queries_file', 'HASURA_BENCH_QUERIES_FILE'),
                ('results_hge_url', 'HASURA_BENCH_RESULTS_HGE_URL'),
                ('upload_root_uri', 'HASURA_BENCH_UPLOAD_ROOT_URI'),
                ('results_hge_admin_secret', 'HASURA_BENCH_RESULTS_HGE_ADMIN_SECRET'),
                ('run_benchmarks', 'HASURA_BENCH_RUN_BENCHMARKS'),
                ('set_scenario_name', 'HASURA_BENCH_SCENARIO_NAME'),
            ])
        self.upload_root_uri = None
        if upload_root_uri:
            p = urlparse(upload_root_uri)
            if p.scheme == 's3':
                # Check if aws credentials are set
                self.get_s3_caller_identity()
            self.upload_root_uri = upload_root_uri



        self.skip_plots = self.parsed_args.skip_plots


class HGEWrkBenchWithArgs(HGEWrkBenchArgs, HGEWrkBench):

    def __init__(self):
        HGEWrkBenchArgs.__init__(self)
        HGEWrkBench.__init__(
            self,
            pg_url = self.pg_url,
            remote_pg_url = self.remote_pg_url,
            pg_docker_image = self.pg_docker_image,
            hge_url = self.hge_url,
            remote_hge_url = self.remote_hge_url,
            hge_docker_image = self.hge_docker_image,
            hge_args = self.hge_args,
            skip_stack_build = self.skip_stack_build,
            graphql_queries_file = self.graphql_queries_file,
            connections = self.connections,
            duration = self.duration
        )

if __name__ == "__main__":
    bench = HGEWrkBenchWithArgs()
    bench.run_tests()

