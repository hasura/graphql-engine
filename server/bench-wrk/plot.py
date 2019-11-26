#!/usr/bin/env python3

import dash
from dash.dependencies import Input, Output, State
import dash_core_components as dcc
import dash_bootstrap_components as dbc
import dash_html_components as html
from urllib.request import urlopen

import json

import sys
import argparse

def as_pairs(l, pair_size=2):
    if len(l) < pair_size:
        return [l]
    else:
        return [l[0:pair_size]] + as_pairs(l[pair_size:], pair_size)


def get_violin_plot_data(bench_results, scenarios):
    x_vals = []
    y_vals = []
    for snro in scenarios:
        def allowed_value(x):
            for k in snro:
                if x[k] != snro[k]:
                    return False
            return True
        req_results = [x for x in bench_results if allowed_value(x)]
        ver_info = snro['version'] or snro['docker_image'].split(':')[1]
        print("Version",ver_info)
        if req_results:
            latencies = urlopen(req_results[0]['latencies_uri']).readlines()
            for x in latencies:
                val_ms = float(x.decode())/1000.0
                y_vals.append(val_ms)
                x_vals.append(ver_info)
    return [{
        'type': 'violin',
        'x': x_vals,
        'y': y_vals,
        'points': False
    }]

def get_histogram_data(bench_results, scenarios):
    print("Getting histogram data")
    data = []
    for snro in scenarios:
        def allowed_value(x):
            for k in snro:
                if x[k] != snro[k]:
                    return False
            return True
        req_results = [x for x in bench_results if allowed_value(x)]
        ver_info = snro['version'] or snro['docker_image'].split(':')[1]
        if req_results:
            histogram = req_results[0]['latency_histogram']
            dataRow = {
                'x' : [1/(1.0-float(x['percentile'])) for x in histogram if float(x['percentile']) < 1],
                'y' : [float(y['latency']) for y in histogram],
                'type': 'line',
                'name': ver_info
            }
            data.append(dataRow)
    return data

def run_dash_server(bench_results):

    bench_results.sort(key=lambda x : x['version'] or x['docker_image'])

    app = dash.Dash(external_stylesheets=[dbc.themes.BOOTSTRAP])

    uniq_queries = list( set( [ x['query_name'] for x in bench_results ] ) )
    children = []

    row_1 = dbc.Row([
        dbc.Col([
            html.Label('Plot type'),
            dcc.Dropdown(
                id='plot-type',
                options=[{'label':q, 'value': q} for q in ['histogram', 'violin plot']],
                value= 'histogram'
            )
        ]),
        dbc.Col([
            html.Label('Query'),
            dcc.Dropdown(
                id='query-name',
                options=[{'label':q, 'value': q} for q in uniq_queries],
                value= uniq_queries[0]
            )
        ]),
        dbc.Col([
            html.Label('Requests/sec'),
            dcc.Dropdown(
                id='rps',
            )
        ])
    ])

    row_2_cols = [
        dbc.Col([
            html.Label('version(s)/docker_image(s)'),
            dcc.Dropdown(
                multi=True
            )
        ])
    ]

    row_2 = dbc.Row(row_2_cols)

    children.append(html.Div(dbc.Container([row_1,row_2])))
    children.append(dcc.Graph(id='graph'))

    app.layout = html.Div(children=children)

    @app.callback(
        Output('ver', 'options'),
        [ Input('query-name', 'value'), Input('rps', 'value') ]
    )
    def updateVerOptions(query_name, rps):
        relvnt_q = [ x for x in bench_results if x['query_name'] ==  query_name and x['requests_per_sec'] == rps ]
        uniq_vers = list(set([
            (x['version'], x['docker_image'])
            for x in relvnt_q
        ]))
        return [
            {
                'label': x[0] or x[1],
                'value': json.dumps({
                    'version': x[0],

                    'docker_image': x[1]
                    })
            }
            for x in uniq_vers
        ]

    @app.callback(
        Output('ver', 'value'),
        [ Input('ver', 'options') ],
        [ State('ver', 'value') ]
    )
    def updateVerValue(options, vers):
        print("Current versions", vers)
        new_vers = []
        allowed_vers = [ o['value'] for o in options]
        default_vers = allowed_vers[:min(2, len(allowed_vers))]
        if not vers:
            print("Returning default")
            return default_vers
        for ver in vers:
            print("Version",ver)
            if ver in allowed_vers:
              new_vers.append(ver)
        if new_vers:
            return new_vers
        else:
            return default_vers


    @app.callback(
        Output('rps', 'options'),
        [ Input('query-name', 'value') ]
    )
    def updateRPSOptions(query_name):
        relvnt_q = [ x for x in bench_results if x['query_name'] ==  query_name ]
        rps = list( set( [x['requests_per_sec'] for x in relvnt_q ] ) )
        return [
            {
                'label':  str(x),
                'value': x
            } for x in sorted(rps)
        ]

    @app.callback(
        Output('rps', 'value'),
        [ Input('rps', 'options') ],
        [ State('rps', 'value') ]
    )
    def updateRPSValue(options, rps):
        values = [ o['value'] for o in options ];
        default_val = values[0]
        if not rps:
            return default_val
        if rps not in values:
            return default_val
        else:
            return rps

    def get_histgoram_figure(scenarios):
        return {
            'data': get_histogram_data(bench_results, scenarios),
            'layout': {
                'yaxis' : {
                    'title': "Latency (ms)"
                },
                'xaxis' : {
                    'title': "Percentile",
                    'type': 'log',
                    'tickvals': [1.0,2.0,10.0,100.0,1000.0,10000.0,10000.0,100000.0],
                    'ticktext': ['0%','50%','90%','99%','99.9%','99.99%','99.999%','99.9999%']
                },
                'title' : 'Latency histogram'
            }
        }

    def get_violin_figure(scenarios):
        return {
            'data': get_violin_plot_data(bench_results, scenarios),
            'layout': {
                'yaxis' : {
                    'title': "Latency (ms)"
                },
                'xaxis': {
                    'title': 'version/docker_image'
                },
                'title': 'Latency'
            }
        }

    @app.callback(
        Output('graph', 'figure'),
        [
            Input('plot-type', 'value'),
            Input('query-name', 'value'),
            Input('rps', 'value'),
            Input('ver', 'value')
        ]
    )
    def updateGraph(plot_type, query_name, rps, vers):
        scenarios = [
            {
                'query_name': query_name,
                'requests_per_sec': rps,
                **json.loads(v)
            }
            for v in set(vers)
        ]
        print(plot_type, scenarios)
        if plot_type == 'histogram':
            return get_histgoram_figure(scenarios)
        elif plot_type == 'violin plot':
            return get_violin_figure(scenarios)

    app.run_server(host="127.0.0.1", debug=False)

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--results', nargs='?', type=argparse.FileType('r'),
        default=sys.stdin)
    args = parser.parse_args()
    bench_results = json.load(args.results)
    print(bench_results)

    print("=" * 20)
    print("starting dash server for graphs")

    run_dash_server(bench_results)
