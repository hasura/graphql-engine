import threading
import websockets
import asyncio
import shutil
import requests
import json
import time
import httpx

#Runs wrk and wrk2 benchmarks for a GraphQL engine based on websocket requests.
#Only one test will be run at a time.

class wrkHTTPSever:

    def __init__(self, graphql_url, graphql_auth_headers={}):
        self.benchmarks_lock = threading.Lock()
        self.graphql_url = graphql_url
        self.graphql_auth_headers = graphql_auth_headers
        self.verify_graphql_url()
        self.verify_if_wrks_are_present()

    def verify_graphql_url(self):
        q = { 'query': 'query { __typename }' }
        r = requests.post(self.graphql_url, json.dumps(q), headers=self.graphql_auth_headers)
        assert r.status_code == 200, 'Could not execute graphql query: ' + q

    def verify_if_wrks_are_present(self):
        for app in ['wrk', 'wrk2']:
            assert shutil.which(app), "Could not find application " + app
    # {  'type' : 'start_benchmark',
    #    'framework' : 'wrk',
    #    'conf': conf
    # }
    async def run_bench(self, websocket, message):
        locked = self.benchmarks_lock.acquire(blocking=False)
        if locked:
            try:
                conf = json.loads(message)
                if conf.get('type') != 'run_bench':
                    return
                appFn = {
                    'wrk': self.run_wrk_bench,
                    'wrk2': self.run_wrk2_bench
                }
                await appFn[conf['framework']](websocket, conf['conf'])
            finally:
                self.benchmarks_lock.release()
        else:
             err = {'error': 'Another benchmark is running'}
             await websocket.send(json.dumps(err))

    def set_arg(self, argName, conf, args):
        opt = '--' + argName
        val = conf.get(argName)
        if val:
            args.extend(opt, str(val))

    def get_wrk_args(self, conf):
        args = [
            '--script', self.lua_dir + '/bench.lua'
        ]
        for argName in ['connections', 'duration', 'threads']:
            self.set_arg(argName, conf, args)
        args.extend([self.graphql_url, conf['query']])
        return args

    wrk2_arg_names = ['connections', 'duration', 'threads', 'rate']

    def get_wrk2_args(self, conf, results_dir):
        args = [
            '--script', self.lua_dir + '/bench-wrk2.lua',
            '--latency'
        ]
        for argName in self.wrk2_arg_names:
            self.set_arg(argName, conf, args)

        args.extend([self.graphql_url, conf['query'], results_dir])
        return args

    async def send_bench_has_started(self, websocket, framework, conf):
        await websocket.send(json.dumps({
            'type': 'status',
            'state':  'running',
            'framework': framework,
            'conf': conf
        }))

    async def send_exit_err_msg(self, websocket, framework, exitcode, stderr):
        err = {
            'type': 'error',
            'framework': framework,
            'error' : {
                'exitcode' : exitcode,
                'message' : stderr.decode()
            }
        }
        await websocket.send(json.dumps(err))

    def gen_results_message(self, framework, conf, results):
        return {
            'type': 'results',
            'framework': {
                'name' : framework,
                'configuration' : conf,
            },
            'results': results
        }

    async def run_wrk_bench(self, websocket, conf):
        self.graphql_exec(conf['query'])

        args = self.get_wrk_args(conf)
        process = await asyncio.create_subprocess_exec(
            'wrk', *args, stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE
        )
        await self.send_bench_has_started(websocket, 'wrk', conf)
        (stdout, stderr) = await process.communicate()
        if process.returncode:
            await self.send_exit_err_msg(websocket, 'wrk', process.returncode, stderr)
        else:
        # TODO the results should have both the input configuration and the results
            result = json.loads(stderr)
            await websocket.send(
                json.dumps(
                    self.gen_results_message('wrk', conf, result)
                )
            )

    async def run_wrk2_bench(self, websocket, conf):
        self.graphql_exec(conf['query'])
        args = self.get_wrk2_args(conf, self.results_dir)
        process = await asyncio.create_subprocess_exec(
            'wrk2', *args, stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE
        )
        await self.send_bench_has_started(websocket, 'wrk2', conf)
        (stdout, stderr) = await process.communicate()
        if process.returncode:
            await self.send_exit_err_msg(websocket, 'wrk2', process.returncode, stderr)
        else:
            # TODO collect the results from wrk test
            # TODO Read the latency values from the latency file
            histogram_file = os.path.join(results_dir, 'latencies.hgrm')
            with open(histogram_file) as f:
                latencies = f.readlines()
            await websocket.send(
                json.dumps(
                    self.gen_results_message('wrk2', conf, result)
                )
            )

    async def graphql_exec(self, query, variables={}):
        req = { 'query': query }
        if variables:
            req['variables'] = variables
        async with httpx.AsyncClient as client:
            resp = await client.post(self.graphql_url, json.dumps(req))
            assert resp.status_code == exp_status, (resp.status_code, resp.json())
            assert 'errors' not in resp.json(), resp.json()
            return resp.json()

    def start_ws_server(self):
        async def handle_ws_request(websocket, path):
            async def run_bench(message):
                await self.run_bench(websocket, message)

            async for message in websocket:
                print(f'< {message}')
                asyncio.get_event_loop().create_task(run_bench(message))

        start_server = websockets.serve(handle_ws_request, "localhost", 8765)

        asyncio.get_event_loop().run_until_complete(start_server)

    # Parse and get the latency histgoram from stdout of wrk2
    def get_latency_histogram(self, result, write_histogram_file):
        const_true = lambda l : True
        state_change = {
            'start' : (
                (lambda l: 'Detailed Percentile spectrum' in l),
                'histogram_start'
            ),
            'histogram_start': (
                (lambda l: 'Value' in l and 'Percentile' in l),
                'histogram_headers'
            ),
            'histogram_headers': (
                const_true,
                'histogram_empty_line'
            ),
            'histogram_empty_line' : (
                const_true,
                'histogram_values'
            ),
            'histogram_values': (
                (lambda l: l.strip().startswith('#')),
                'histogram_summary'
            ),
            'histogram_summary': (
                (lambda l: not l.strip().startswith('#')),
                'histogram_end'
            )
        }

        def next_state(state, line):
            (check, next_state) = state_change[state]
            return next_state if check(line) else state

        state = 'start'
        histogram = []
        raw = ""
        print(Fore.CYAN + "Latency histogram summary" + Style.RESET_ALL)
        for line in result.splitlines():
            state = next_state(state, line)
            # Change the state
            if state == 'start':
                continue
            elif state == 'histogram_end':
                break
            if state in ['histogram_headers','histogram_values','histogram_summary']:
                raw.append(line + '\n')
            if state == 'histogram_values':
                (val, percentile, total_count, _) = line.strip().split()
                histogram.append({
                    'percentile': float(percentile),
                    'latency': float(val),
                    'total_count': float(total_count)
                })
        return (histogram, raw)

if __name__ == '__main__':
    server = wrkHTTPSever('http://localhost:8083/v1/graphql')
    server.start_ws_server()
    asyncio.get_event_loop().run_forever()
